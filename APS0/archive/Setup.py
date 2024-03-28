import os 
EXPECTED_RESULTS_DIR = './expected'
RESULTS_DIR = './results'
SAMPLES_DIR = './Samples'
SUB_DIRS =['defs','exprs','prim']
def initial_setup():
    for dir in SUB_DIRS: 
        os.makedirs(f'{EXPECTED_RESULTS_DIR}/{dir}')
    for dir in SUB_DIRS: 
        os.makedirs(f'{RESULTS_DIR}/{dir}')
        
    # ! Creating the subdirs for results
    for dir in SUB_DIRS: 
        for file in os.listdir(f'{SAMPLES_DIR}/{dir}') : 
            file_name = file.split('.')[0]
            file_path = f'{SAMPLES_DIR}/{dir}/{file}'
            expected_result_dir = f'{EXPECTED_RESULTS_DIR}/{dir}/{file_name}'
            result_dir = f'{RESULTS_DIR}/{dir}/{file_name}'
            os.makedirs(expected_result_dir)
            expected_type_result_file_path = f'{expected_result_dir}/type.result'
            expected_eval_result_file_path = f'{expected_result_dir}/eval.result'
            f=open(expected_type_result_file_path,'w')
            f=open(expected_eval_result_file_path,'w')
        