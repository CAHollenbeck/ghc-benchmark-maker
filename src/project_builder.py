import argparse
import os
import sys
import subprocess
import time
from useful_things import expdir, datadir, do_cmd, logdir, logerr, loggit, tempdir


###################################################################
#
#
# This script builds the projects in the folder specified.
# Note: Building may take a VERY long time. This is ok.
# This script figures out which projects have already been built
# and builds the remaining projects.
#
# NOTE: This script will record and save the time it took to build
# each project and store it in the build_successes file.
#
# Usage: python3 project_builder.py <project_folder> <compiler>
#
###################################################################

# buildfail :: String -> String -> IO Void
#
# Appends project names to the failed build file.
def buildfail(pname, errmsg, failurefile):
  with open(failurefile, 'a+') as f:
      f.write(pname + "\n")

# NOTES:
# Make sure projdir is a full path

# build_projects
#
# projdir : Directory of projects to be built
# compiler : Compiler to build & test the projects
def build_projects(projdir, compiler):
  # Print usage message if wrong number of args passed
  if len(sys.argv) != 3:
    print(usage_msg)
    sys.exit(1)
  
  prefix = ""
  newest_ghc = True
  
  if not os.path.isdir(projdir):
    msg = "Invalid project folder: " + projdir
    loggit(msg)
    sys.exit()

  all_projects = None
  successfile  = None
  failurefile  = None 
  np_string    = ""
  spaces       = "    "
  suc_str  = prefix + "build_successes_"
  fail_str = prefix + "build_failures_"
  txt      = ".txt"
  tmppath = os.path.normpath(projdir)
  dirbase = os.path.basename(tmppath)
  # Files to keep track of all projects that have attempted build,
  # to avoid repeating work if the build is interrupted
  successfile = os.path.join(logdir, suc_str + dirbase + txt)
  failurefile = os.path.join(logdir, fail_str + dirbase + txt)
  
  projectfile = os.path.join(logdir, suc_str + dirbase + txt)
  # Just build all the projects in the given directory
  all_projects = os.listdir(projdir)
  
  # Contains a list of projects already passed
  already_processed = []
  
  # Retrieve projects already successfully built, as written to file.
  # Also retrieve projects unsuccesfully built, as written to file.
  # Append all of these project names together into a list--we don't want
  # to try to build them again if the script gets interrupted and restarts.
  if os.path.exists(successfile):
      with open(successfile) as f: already_processed += f.read().splitlines()
  if os.path.exists(failurefile):
      with open(failurefile) as f: already_processed += f.read().splitlines()
  
  # If all projects to be built appear in the already-processed list,
  # inform that all projects have been built and exit.
  projects = [p for p in all_projects if (p not in already_processed)]
  if not projects:
      print("All projects built.\n")
      sys.exit()
  
  # Commands to clean & build the projects
  cmd_clean = "cabal new-clean"
  #cmd_configure = "cabal new-configure"
  cmd_build = "cabal new-build all --with-compiler=" + compiler
  
  # Attempt to build each project...
  for projname in projects:
    loggit(spaces + np_string + "Building " + projname + "...")
    projectpath = os.path.join(projdir, projname)
    try:
      os.chdir(projectpath) # Change in to project dir
     
      # Cabal clean
      do_cmd(cmd_clean)
  
      # Start a subprocess to build the project.
      # Retrieve status information from the process.
      starttime = time.time()
      p2 = subprocess.Popen(cmd_build, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
      p2sto, p2ste = p2.communicate()
      endtime = time.time()
      total_time = endtime - starttime
  
      # Write to failed projects file if build fails.
      if p2ste: logerr("FAIL BUILD on " + projname + "\n" + p2ste.decode())
      
      # If success (status 0), write to success file.
      if p2.returncode == 0:
          loggit(spaces + "Successfully built " + projname)
          with open(successfile, 'a+') as f: # Write file name and time to success file
              f.write(projname + ", " + str(total_time) + "\n")
      else: # Log build failure to the log file
        loggit(spaces + "Failed to build " + projname)
        buildfail(projname, "Return code non-zero", failurefile)
    except Exception as e: # Log exceptions during building to the log file. Continue building.
        loggit(spaces + "Exception while building " + projname + ":\n" + str(e))
        buildfail(projname, str(e), failurefile)
  
    os.chdir(expdir) # Just in case, change back to expdir
  
  # Change dir back to working dir
  os.chdir(expdir)
  return(successfile) # Return string of location of built successes.

# To call the project_builder directly from command line
if __name__ == "__main__":
  parser = argparse.ArgumentParser(description="""This script builds the projects in the folder specified.$
   Note: Building may take a long time. If interrupted, this script figures out which projects have already been built
   and builds the remaining projects.""")
  parser.add_argument("projdir", type=str, help="Directory containing Haskell projects to be built.")
  parser.add_argument("compiler", type=str, help="Compiler to build & test the projects.")
  args = parser.parse_args()
  projdir = args.projdir
  compiler = args.compiler
  if not os.path.isdir(projdir):
    print("Invalid project folder: " + projdir)
    sys.exit()
  build_projects(projdir, compiler)
