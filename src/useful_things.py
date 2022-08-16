import os
import subprocess

parentdir = os.path.abspath(os.path.join(os.getcwd(), os.pardir))
expdir = os.path.join(parentdir, "EXPERIMENTS")
datadir = os.path.join(expdir, "DATA")
logdir  = os.path.join(expdir, "LOGS")
logfile = os.path.join(logdir, "logs.txt")
errfile = os.path.join(logdir, "error_logs.txt")
origdir = os.path.join(expdir, "ORIGINAL_PROJECTS")
running = "Running: "
timedir = os.path.join(datadir, "TIMES")
tempdir = os.path.join(datadir, "TEMP")
curled_project_file = os.path.join(logdir, "projects_attempted_curl.txt")

if not os.path.exists(expdir):
    os.mkdir(expdir)

if not os.path.exists(logdir):
    os.mkdir(logdir)

# Write the thing to the log file for record keeping
def loggit(m):
    with open(logfile, 'a+') as f:
        f.write(m + "\n")

# Write errors to a file
def logerr(m):
    with open(errfile, 'a+') as f:
        f.write(m + '\n')

def do_cmd(cmd):
    loggit("CMD: " + cmd)
    try:
        subprocess.check_call(cmd, timeout=40, shell=True)
    except subprocess.CalledProcessError as e:
        logerr("Failed command: " + cmd + "\nwith error code: " + str(e.returncode) + "\n")
