import shutil,os

EXPECTED_RESULTS_DIR = './expected'
RESULTS_DIR = './results'
SAMPLES_DIR = './Samples'
SUB_DIRS =['cmds','defs','exprs','prim','stat']

def initial_setup():
    source_dir = "../APS1/expected"
    destination_dir = EXPECTED_RESULTS_DIR
    shutil.copytree(source_dir, destination_dir)


    for dir in SUB_DIRS : 
        if not os.path.exists(f'{EXPECTED_RESULTS_DIR}/{dir}'):
            os.makedirs(f'{EXPECTED_RESULTS_DIR}/{dir}')
            

    for dir in SUB_DIRS : 
        for file in os.listdir(f'{SAMPLES_DIR}/{dir}') :
            file_name = file.split('.')[0] 
            expected_result_dir =f'{EXPECTED_RESULTS_DIR}/{dir}/{file_name}'
            print(expected_result_dir,os.path.exists(expected_result_dir))
            if os.path.exists(expected_result_dir):
                continue
            os.makedirs(expected_result_dir)
            expected_type_result_file_path = f'{expected_result_dir}/type.result'
            expected_eval_result_file_path = f'{expected_result_dir}/eval.result'
            f=open(expected_type_result_file_path,'w')
            f=open(expected_eval_result_file_path,'w')