
# Imports
import itertools
import copy
import numpy as np
import pandas as pd
import xml.etree.ElementTree as ET

# Handles both uncertain factors and levers
#'climate_scen':{'Global_GCM': ['NorESM1-M', 'GFDL-ESM2M', 'MIROC-ESM-CHEM', 'IPSL-CM5A-LR', 'HadGEM2-ES'],
#                'Global_RCP': ['rcp2p6', 'rcp4p5', 'rcp6p0', 'rcp8p5']},

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
        'Uncertainty_5': ['Low', 'High']
          }

num_xl_factors = len(XL_fac.keys())
num_experiments = 1
for key in XL_fac:
    num_experiments *= len(XL_fac[key])
# Read in base configuration file; get some basic details from the file
base_dir = 'D:/INFEWS/RDM/IDB_RDM_Colombia/'
base_pic_dir = '../input/'
config_dir = base_dir + 'pic/'
scenarios = ['DDP_XL_LUCPol']
num_scenarios = len(scenarios)
# Specify Pandas DF to store details related to design of experiment
add_on_DF_columns = XL_fac.keys()
std_cols = ['scenario', 'experiment']
DOE_DF = pd.DataFrame(columns = std_cols + list(XL_fac),
                      index = [i for i in range(0, num_scenarios*num_experiments)])
scenario_counter = 0  # Aids in cases where we have to handle more than one scenario.
for gcam_scen in scenarios:
    base_config_file = 'RDM_' + gcam_scen + '.xml'  #  'configuration_LAC.xml'
    tree=ET.parse(config_dir + base_config_file)
    root=tree.getroot()  # Element "Configuration" top of file
    scenComp=root.find('ScenarioComponents')  # Element "ScenarioComponents"

    # Create unique combinations of uncertain factors (xml files) for which to run GCAM
    factor_files = [[] for i in range(len(XL_fac.keys()))]
    factor_names = [[] for i in range(len(XL_fac.keys()))]
    factor_levels = [[] for i in range(len(XL_fac.keys()))]
    impacts_ag_water = {}
    for key in XL_fac:
        print(key)
        if key not in ['climate_scen']:
            for item in XL_fac[key]:
                factor_files[list(XL_fac.keys()).index(key)].append(base_pic_dir + 'idb_5.3/rdm/XL_category_files/' + key + '_' + item +
                                                              '.xml')
                factor_names[list(XL_fac.keys()).index(key)].append(key)
                factor_levels[list(XL_fac.keys()).index(key)].append(item)
        else:
            for gcm in XL_fac['climate_scen']['Global_GCM']:
                for rcp in XL_fac['climate_scen']['Global_RCP']:
                    filepath = base_pic_dir + 'idb/impacts/Hydro/hydro_impacts_' + gcm + '_' + rcp + '.xml'
                    print(filepath)
                    factor_files[XL_fac.keys().index(key)].append(filepath)
                    water_filepath = base_pic_dir + 'idb/impacts/Water/runoff_impacts_' + gcm+ '_' + rcp + '.xml'
                    ag_filepath = base_pic_dir + 'idb/impacts/Ag/ag_prodchange_' + gcm + '_' + rcp + '.xml'
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
    DOE_DF.to_csv(base_dir + 'uncertainty/' + 'DOE_XLRM_' + gcam_scen + '.csv', index=False)
    scenario_counter += 1
    # convert list of tuples to list of lists
    for tup in range(len(factor_file_list_orig)):
        factor_file_list_orig[tup] = list(factor_file_list_orig[tup])
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
        for column in range(len(factor_file_list_orig2[0])):
            factor_file_list_orig2[row][column] = ET.SubElement(scenComp, 'Value')
            factor_file_list_orig2[row][column].attrib = {'name':'uncertainty_combination_elem'+str(column)}
            factor_file_list_orig2[row][column].text = factor_file_list_orig[row][column]  # + '\n'
        root[0][4].text = '../../output/FinalRuns/IDB_RDM/uncertainty_' + scen  # Change output database location
        xml_text = base_dir + 'uncertainty/config_files/LUCPol/' + 'RDM_' + gcam_scen + '_' + scen + '.xml'
        # parser = etree.XMLParser(remove_blank_text=True)
        tree.write(xml_text)  # , pretty_print = True
        # Void added items so they don't appear in subsequent XML files.
        for column in range(len(factor_file_list_orig2[0])):
            scenComp.remove(factor_file_list_orig2[row][column])
