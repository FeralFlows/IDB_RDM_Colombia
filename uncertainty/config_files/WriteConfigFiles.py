# Imports
import itertools
import copy
import numpy as np
import xml.etree.ElementTree as ET

# Uncertain factors
# Deal with climate uncertainty separately first. Given file naming convention is separate for water and ag impact
# files, need to handle that here

#'Global_EV_cost': ['Low', 'Mid', 'High']
#'Global_Renewables_cost': ['Low', 'Mid', 'High']

X_fac = {'Colombia_Population': ['Low', 'Mid', 'High'],
        'Colombia_GDP': ['Low', 'Mid', 'High'],
        'climate_scen':
             {
        'Global_GCM': ['NorESM1-M', 'GFDL-ESM2M', 'MIROC-ESM-CHEM', 'IPSL-CM5A-LR', 'HadGEM2-ES'],
        'Global_RCP': ['rcp2p6', 'rcp4p5', 'rcp6p0', 'rcp8p5']
             }
         }
# Read in base configuration file; get some basic details from the file
base_dir = 'C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/'
base_pic_dir = '../input/'
config_dir = base_dir + 'pic/'
scenarios = ['Reference', 'ColPol', 'DDP']
for gcam_scen in scenarios:
    base_config_file = 'RDM_' + gcam_scen + '.xml'  #  'configuration_LAC.xml'
    tree=ET.parse(config_dir + base_config_file)
    root=tree.getroot()  # Element "Configuration" top of file
    scenComp=root.find('ScenarioComponents')  # Element "ScenarioComponents"

    # Create unique combinations of uncertain factors (xml files) for which to run GCAM
    factor_files = [[] for i in range(len(X_fac.keys()))]
    impacts_ag_water = {}
    for key in X_fac:
        if key not in ['climate_scen']:
            for item in X_fac[key]:
                factor_files[X_fac.keys().index(key)].append(base_pic_dir + 'idb/rdm/' + key + '_' + item + '.xml')
        else:
            for gcm in X_fac['climate_scen']['Global_GCM']:
                for rcp in X_fac['climate_scen']['Global_RCP']:
                    filepath = base_pic_dir + 'idb/impacts/Hydro/hydro_impacts_' + gcm + '_' + rcp + '.xml'
                    factor_files[X_fac.keys().index(key)].append(filepath)
                    water_filepath = base_pic_dir + 'idb/impacts/Water/runoff_impacts_' + gcm+ '_' + rcp + '.xml'
                    ag_filepath = base_pic_dir + 'idb/impacts/Ag/ag_prodchange_' + gcm + '_' + rcp + '.xml'
                    impacts_ag_water[filepath] = [water_filepath, ag_filepath]

    factor_file_list_orig = list(itertools.product(*factor_files))
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
            factor_file_list_orig2[row][column] = ET.SubElement(scenComp,tag='Value')
            factor_file_list_orig2[row][column].attrib = {'name':'uncertainty_combination_elem'+str(column)}
            factor_file_list_orig2[row][column].text = factor_file_list_orig[row][column]  # + '\n'
        root[0][4].text = '../../output/FinalRuns/IDB_RDM/uncertainty_' + scen  # Change output database location
        tree.write(base_dir + 'uncertainty/config_files/output/' 'RDM_' + gcam_scen + '_' + scen + '.xml')  # Write
        # XML
        # file
        # Void added items so they don't appear in subsequent XML files.
        for column in range(len(factor_file_list_orig2[0])):
            scenComp.remove(factor_file_list_orig2[row][column])
