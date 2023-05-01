import argparse
import os
import glob
import csv
import pandas as pd

def gcam_output_csv_inventory(output_dir, total_jobs):
    '''
    Identifies which GCAM runs have not finished, based on presence or absence of csv output
    :param output_dir: path to output dir where .csv output files are
    :type output_dir: str
    :param total_jobs: number of GCAM scenarios run
    :type total_jobs: str
    :return:
    '''

    file_names = [os.path.basename(x) for x in glob.glob(os.path.join(output_dir, '*.csv'))]
    scenarios_run_target = ['RDM_Policy_{}'.format(str(i)) for i in range(int(total_jobs))]
    scenarios_run_actual = []
    for file in file_names:
        file_path = os.path.join(output_dir, file)
        with open(file_path) as f:
            scenario_name = f.readlines()[2].split(',')[0].strip('"')
            scenarios_run_actual.append(scenario_name)
        f.close()
    incomplete_runs = set(scenarios_run_actual).symmetric_difference(set(scenarios_run_target))
    incomplete_runs = list(incomplete_runs)
    inc_runs_df = pd.DataFrame(incomplete_runs)
    inc_runs_df.columns = ['file_name']
    print('Number of GCAM runs that did not complete: {}'.format(len(incomplete_runs)))
    inventory_dir = os.path.join(output_dir, 'inventory')
    os.makedirs(inventory_dir, exist_ok=True)
    inventory_file = os.path.join(inventory_dir, 'inventory.csv')

    inc_runs_df.to_csv(inventory_file, index = False, header = False, sep = " ")
    #with open(inventory_file, 'w', newline="") as f:
    #    write = csv.writer(f)
    #    write.writerow(incomplete_runs)

    return incomplete_runs

def slurm_out_inventory(output_dir, total_jobs):
    '''
    Identifies which model periods failed from GCAM .out file(s)
    :param output_dir: path to output dir where *.out files are
    :param total_jobs: how many gcam runs were done (e.g., 8192)
    :return:
    '''

    scenario_names = ['RDM_Policy_{}'.format(str(i)) for i in range(int(total_jobs))]
    df = pd.DataFrame(scenario_names)
    df.columns = ['scenario_name']
    df['failed_periods'] = 'None'
    df['colombia_mkt_fail'] = 0
    df['co_mkt_fail_dtls'] = None
    df['colombia_ghg_mkt_fail'] = None
    df['CO_GHG_supply'] = None
    df['CO_GHG_demand'] = None    
    file_names = [os.path.basename(x) for x in glob.glob(os.path.join(output_dir, '*.out'))]
    include = {scenario: 0 for scenario in scenario_names}  # will fill in for runs producing csv
    for file in file_names:
        err = []
        print("Processing file: {}".format(file))
        file_path = os.path.join(output_dir, file)
        with open(file_path) as f:
            # Get scenario name
            for line in f:
                if 'Starting new scenario:' in line:
                    scenario = line.split(": ")[1].strip("\n").strip('"').strip("'")
                    print('Processing Scenario: {}'.format(scenario))
                    break
            f.close()

        # See that this run actually produced csv output. If not, don't include in inventory.
        with open(file_path) as f:
            for line in f:
                if 'INFO; Finished running batch file' in line:
                    include[scenario] = 1
                    break
            f.close()
        failed_periods = None
        if include[scenario] == 1:
            # Locate model time periods that did not solve
            with open(file_path) as f:
                for line in f:
                    if 'The following model periods did not solve:' in line:
                        failed_periods = line.split(": ")[1].strip("\n").strip('"').strip("'")
                        print('Failed time periods: {}'.format(failed_periods))
                        # strip any trailing comma if it exists
                        if failed_periods[-2:] == ', ':
                            failed_periods = failed_periods[0:-2]
                        break
                df.loc[(df.scenario_name == scenario), 'failed_periods'] = failed_periods
                f.close()
            # For the time periods that did not solve, record if Colombia failed to solve, 
            # and for which markets
            if failed_periods is not None:
                failed_period_list = failed_periods.split(", ")
                
                # We have at least one failed model period
                with open(file_path) as f:
                    for line in f:
                        if 'ERROR' in line:
                            if "Colombia" in line:
                                err.append(line.split(", ")[-2])
                                if "GHG" in line:
                                    df.loc[(df.scenario_name == scenario), 'colombia_ghg_mkt_fail'] = 1
                                    df.loc[(df.scenario_name == scenario), 'CO_GHG_supply'] = line.split(", ")[-5]
                                    df.loc[(df.scenario_name == scenario), 'CO_GHG_demand'] = line.split(", ")[-4]
                    if len(err) > 0:
                        print(err)
                        df.loc[(df.scenario_name == scenario), 'colombia_mkt_fail'] = 1
                        df.loc[(df.scenario_name == scenario), 'co_mkt_fail_dtls'] = '; '.join(err)
                    f.close() 
                
    inventory_dir = os.path.join(output_dir, 'inventory')
    os.makedirs(inventory_dir, exist_ok=True)
    inventory_file = os.path.join(inventory_dir, 'gcam_fail_inventory.csv')
    df.to_csv(inventory_file, index = False, header = False)


def rename_files(output_dir, gcam_meta_scenario):
    '''
    Renames files so that scenario name in file name matches data contained in file.
    :param output_dir: path to output dir where .csv output files are
    :type output_dir: str
    :param gcam_meta_scenario: name of scenario, which will appear at start of each 
    output csv file
    :type gcam_meta_scenario: str
    :return: Nothing. Files get renamed, there's no data product.
    '''

    file_names = [os.path.basename(x) for x in glob.glob(os.path.join(output_dir, '*.csv'))]
    scenarios_run_actual = []
    for file in file_names:
        file_path = os.path.join(output_dir, file)
        with open(file_path) as f:
            scenario_name = f.readlines()[2].split(',')[0].strip('"')
        f.close()
        # Check if file needs to be renamed
        print('actual scenario name is: ' + file.split('.csv')[0])
        print('file scenario name is: ' + scenario_name)
        if scenario_name != file.split('.csv')[0]:
            print('entering this part of script')
            new_filename = scenario_name + '_rn.csv'
            new_filepath = os.path.join(output_dir, new_filename)
            os.rename(file_path, new_filepath)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='inventory completed scenario csv files to identify failed GCAM runs')
    parser.add_argument('--output_dir', type=str,
                        help='see output_inventory.py gcam_output_csv_inventory() function docstring')
    parser.add_argument('--total_jobs', type=str,
                        help='see output_inventory.py gcam_output_csv_inventory() function docstring')
    parser.add_argument('--inventory', type=int,
                        help='1 runs gcam_output_csv_inventory, 2 runs slurm_out_inventory, 3 rename files')    
    parser.add_argument('--gcam_meta_scenario', type=str,
                        help='scenario name that will appear in all gcam output csv files')                            
    args = parser.parse_args()
    if args.inventory == 1:
        gcam_output_csv_inventory(args.output_dir, args.total_jobs)
    elif args.inventory == 2:
        slurm_out_inventory(args.output_dir, args.total_jobs)    
    elif args.inventory == 3:
        rename_files(args.output_dir, args.gcam_meta_scenario)
