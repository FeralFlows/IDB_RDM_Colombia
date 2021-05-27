# Please use Python 3.8 to run this script

# Imports
import itertools
import argparse
import os
import copy
import numpy as np
import pandas as pd
import xml.etree.ElementTree as ET

# Handles both uncertain factors and levers
def indent(elem, level=0):
    i = "\n" + level*"  "
    if len(elem):
        if not elem.text or not elem.text.strip():
            elem.text = i + "  "
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
        for elem in elem:
            indent(elem, level+1)
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
    else:
        if level and (not elem.tail or not elem.tail.strip()):
            elem.tail = i


def main(scenarios, base_dir, base_gcam_dir, base_config_file, base_alt_xml_dir, output_dir, XL_fac, XL_cat):
    ''' Generate GCAM config files for use in Scenario Discovery and RDM experiments.
    :param scenarios: list of strings of scenario names
    :type scenarios: list of str
    :param base_dir: path to where you cloned the IDB_RDM_Colombia repo
    :type base_dir: str
    :param base_gcam_dir: path to GCAM dir (contains /exe, /input, etc.)
    :type base_gcam_dir: str
    :param base_config_file: path to config file that will serve as basis of alternative files to be generated
    :type base_config_file: str
    :param output_dir: path where output files will be placed
    :type output_dir: str
    '''


    num_xl_factors = len(XL_fac.keys())
    num_experiments = 1
    for key in XL_fac:
        num_experiments *= len(XL_fac[key])
    # Read in base configuration file; get some basic details from the file
    config_dir = os.path.join(base_dir, 'relationships', 'gcam', 'config')
    num_scenarios = len(scenarios)
    # Specify Pandas DF to store details related to design of experiment
    add_on_DF_columns = XL_fac.keys()
    std_cols = ['scenario', 'experiment']
    DOE_DF = pd.DataFrame(columns = std_cols + list(XL_fac),
                          index = [i for i in range(0, num_scenarios*num_experiments)])
    scenario_counter = 0  # Aids in cases where we have to handle more than one scenario.
    for gcam_scen in scenarios:
        tree=ET.parse(os.path.join(config_dir, 'input', base_config_file))
        root=tree.getroot()  # Element "Configuration" top of file
        scenComp=root.find('ScenarioComponents')  # Element "ScenarioComponents"

        # Create unique combinations of uncertain factors (xml files) for which to run GCAM
        factor_files = [[] for i in range(len(XL_fac.keys()))]
        factor_names = [[] for i in range(len(XL_fac.keys()))]
        factor_levels = [[] for i in range(len(XL_fac.keys()))]
        impacts_ag_water = {}
        for key in XL_fac:
            if key not in ['climate_scen']:
                for item in XL_fac[key]:
                    factor_names[list(XL_fac.keys()).index(key)].append(key)
                    factor_levels[list(XL_fac.keys()).index(key)].append(item)
                    key_item = key + '_' + item
                    if key_item in XL_cat:
                        append_str = ''
                        for cat in XL_cat[key_item]:
                            append_str += str(base_alt_xml_dir + key_item + '_' + cat + '.xml;')
                        factor_files[list(XL_fac.keys()).index(key)].append(append_str)
                    else:
                        factor_files[list(XL_fac.keys()).index(key)].append('')
            else:
                for gcm in XL_fac['climate_scen']['Global_GCM']:
                    for rcp in XL_fac['climate_scen']['Global_RCP']:
                        filepath = base_gcam_dir + 'idb/impacts/Hydro/hydro_impacts_' + gcm + '_' + rcp + '.xml'
                        factor_files[XL_fac.keys().index(key)].append(filepath)
                        water_filepath = base_gcam_dir + 'idb/impacts/Water/runoff_impacts_' + gcm+ '_' + rcp + '.xml'
                        ag_filepath = base_gcam_dir + 'idb/impacts/Ag/ag_prodchange_' + gcm + '_' + rcp + '.xml'
                        impacts_ag_water[filepath] = [water_filepath, ag_filepath]

        factor_file_list_orig = list(itertools.product(*factor_files))
        factor_names_list = list(itertools.product(*factor_names))
        factor_levels_list = list(itertools.product(*factor_levels))

        # Store (in DOE_DF) factor names and levels associated with this experiment
        # Set scenario values
        DOE_DF.iloc[scenario_counter:scenario_counter+num_experiments].scenario = gcam_scen

        for exp in range(num_experiments):
            for fac_num in range(num_xl_factors):
                DOE_DF.iloc[scenario_counter*num_experiments + exp][factor_names_list[exp][fac_num]] = \
                    factor_levels_list[exp][fac_num]
                DOE_DF.iloc[scenario_counter * num_experiments + exp].experiment = exp
        # Save XLRM Design of Experiment sheet
        DOE_DF.to_csv(os.path.join(output_dir, 'doe', 'DOE_XLRM_' + gcam_scen + '.csv'), index=False)
        scenario_counter += 1
        # convert list of tuples to list of lists
        for tup in range(len(factor_file_list_orig)):
            factor_file_list_orig[tup] = list(factor_file_list_orig[tup])
            factor_file_list_orig[tup] = [xml for comb_xml in factor_file_list_orig[tup] for xml in comb_xml.split(';')]
            factor_file_list_orig[tup] = [string for string in factor_file_list_orig[tup] if string != ""]
        # substitute in additional impacts files for water and ag
        for unique_file_combo in range(len(factor_file_list_orig)):
            for filepath in factor_file_list_orig[unique_file_combo]:
                try:
                    factor_file_list_orig[unique_file_combo].append(impacts_ag_water[filepath][0])
                    factor_file_list_orig[unique_file_combo].append(impacts_ag_water[filepath][1])
                except KeyError:
                    pass

        factor_file_list_orig2 = copy.deepcopy(factor_file_list_orig)
        factor_file_list = copy.deepcopy(factor_file_list_orig)
        # Add column to store unique simulation run ID for each row
        for row in range(len(factor_file_list)):
            scen = (str(row))  # Scenario name
            factor_file_list[row] = list(factor_file_list[row])  # first convert each tuple of file names to list
            factor_file_list_orig2[row] = list(factor_file_list_orig2[row])  # first convert each tuple of file names to list
            factor_file_list[row].append(row)  # store a unique simulation ID with each file combination
            root[2][0].text = gcam_scen + '_' + scen  # Scenario name
            for column in range(len(factor_file_list_orig2[row])):
                factor_file_list_orig2[row][column] = ET.SubElement(scenComp, 'Value')
                factor_file_list_orig2[row][column].attrib = {'name':'uncertainty_combination_elem'+str(column)}
                factor_file_list_orig2[row][column].text = factor_file_list_orig[row][column] # + '\n'
            root[0][4].text = '../../output/FinalRuns/IDB_RDM/uncertainty_' + scen  # Change output database location
            xml_text = os.path.join(output_dir, 'xml', 'RDM_' + gcam_scen + '_' + scen + '.xml')
            # parser = etree.XMLParser(remove_blank_text=True)
            indent(root)
            tree.write(xml_text)  # , pretty_print = True
            # Void added items so they don't appear in subsequent XML files.
            for column in range(len(factor_file_list_orig2[row])):
                scenComp.remove(factor_file_list_orig2[row][column])


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Generate GCAM configuration files')
    parser.add_argument("--scenarios", nargs='+',
                        help='see gcam_config_generator.py main() function docstring')
    parser.add_argument('--base_dir', type=str,
                        help='see gcam_config_generator.py main() function docstring')
    parser.add_argument('--base_gcam_dir', type=str,
                        help='see gcam_config_generator.py main() function docstring')
    parser.add_argument('--base_config_file', type=str,
                        help='see gcam_config_generator.py main() function docstring')
    parser.add_argument('--base_alt_xml_dir',type=str,
                        help='see gcam_config_generator.py main() function docstring')
    parser.add_argument('--output_dir', type=str,
                        help='see gcam_config_generator.py main() function docstring')

    args = parser.parse_args()

    # For now, the XL_fac and XL_cat are included here rather than parsed from shell script.

    XL_fac = {'Strategy_1': ['Low', 'High'],
              'Strategy_2': ['Low', 'High'],
              'Strategy_3': ['Low', 'High'],
              'Strategy_4': ['Low', 'High'],
              'Strategy_5': ['Low', 'High'],
              'Strategy_6': ['Low', 'High'],
              'Uncertainty_1': ['Low', 'High'],
              'Uncertainty_2': ['Low', 'High'],
              'Uncertainty_3': ['Low', 'High'],
              'Uncertainty_4': ['Low', 'High'],
              'Uncertainty_5': ['Low', 'High'],
              'Uncertainty_6': ['Low', 'High'],
              }

    XL_cat = {'Strategy_1_High': ['RPS', 'Nuclear'],
              'Strategy_1_Low': ['RPS', 'Nuclear'],
              'Strategy_2_High': ['BldEE', 'IndEE'],
              'Strategy_2_Low': ['BldEE', 'IndEE'],
              'Strategy_3_High': ['ElecTrans'],
              'Strategy_3_Low': ['ElecTrans'],
              'Strategy_4_High': ['PublicTrans'],
              'Strategy_4_Low': ['PublicTrans'],
              'Strategy_5_High': ['AFOLU'],
              'Strategy_6_High': ['Meat'],
              'Uncertainty_1_High': ['GDP', 'Population'],
              'Uncertainty_1_Low': ['GDP', 'Population'],
              'Uncertainty_2_High': ['EVCost'],
              'Uncertainty_2_Low': ['EVCost'],
              'Uncertainty_3_High': ['RECostSolar', 'RECostWind'],
              'Uncertainty_4_High': ['CCSCost'],
              'Uncertainty_4_Low': ['CCSCost'],
              'Uncertainty_5_High': ['Ag', 'Hydro', 'Runoff'],
              'Uncertainty_5_Low': ['Ag', 'Hydro', 'Runoff'],
              'Uncertainty_6_High': ['HOV-CL']
              }
    print(args.scenarios)
    print(type(args.scenarios))
    main(args.scenarios, args.base_dir, args.base_gcam_dir, args.base_config_file, args.base_alt_xml_dir,
         args.output_dir, XL_fac, XL_cat)
