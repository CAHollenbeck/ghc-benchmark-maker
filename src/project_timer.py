import argparse
import glob
import os
from pathlib import Path
import re
import subprocess
import sys
import time
from useful_things import do_cmd, expdir, datadir, loggit, logerr


##################################################################################
#
# For each project in successful builds (as indicated by <project-list>), this 
# script times the project with specified GHC passed and stores the time in a
# batch of CSV files in the time directory.
#
# <projdir> : Directory containing projects
# <compiler> : Compiler to use
# <project-list> : [Optional] List of projects to time (successfully built)
#
##################################################################################


def time_projects(projdir, compiler, projlist=None):
  projectfile = "" # Global -- File containing list of projects to time
  projectlist = [] # Global -- List to hold project names
  compiler = compiler # Global -- Compiler to use
  batchsize   = 50 # Number of projects per CSV
  timings = 11 # How many timings to do for each project

  projbase = os.path.basename(projdir)
  timedir = os.path.join(expdir, "TIMES_" + str(projbase))

  if projlist:
    if not os.path.isfile(projlist):
      logerr("project_timer: Invalid project file: " + projectfile)
      return
    with open(projectfile) as f: projectlist = f.read().splitlines()
  else:
    if not os.path.isdir(projdir):
        logerr("project_timer: Invalid directory: " + projdir)
    projectlist = os.listdir(projdir) # Just time all projects in directory 
  
  # Initialize the indices of the batch of projects to be timed
  startindices = [0]
  endindices   = [len(projectlist)]

  # Make the time directory if it doesn't exist
  if not os.path.exists(timedir):
    # Make the time file
    loggit("Creating TIMES folder")
    os.mkdir(timedir)

  timesfile   = ""
  successfile = ""
  failurefile = ""

  # Timed CSVs already created
  timedfiles = []

  # The number of batches already completed & saved
  # This will be determined below.
  maxbatch = 0

  # Get the string to name the batch by taking the last folder in the
  # directory containing the projects to be timed.
  tmpstring = os.path.normpath(projdir)
  batchname = os.path.basename(tmpstring)

  # Set names of files to write successful/unsuccessful project timings to.
  # These files will simply list projects that succeeded or failed to time.
  successfilename = batchname + "_timed_successes.txt"
  failurefilename = batchname + "_timed_failures.txt"
  successfile = os.path.join(datadir, successfilename) # file with names of successfully timed projects
  failurefile = os.path.join(datadir, failurefilename) # file with names of unsuccessfully timed projects

  # Grab the names of time CSV files that have already been created.
  timedfiles = glob.glob(os.path.join(timedir, batchname + "_project_times_*")) # glob to grab times already done

  # String (incomplete) for the file to write the current times to
  timesfile = os.path.join(timedir, batchname + "_project_times_")

  # Determine how many batches of timed files there are.
  for f in timedfiles: # Reading in existing time file names and finding the highest integer
    basename = os.path.basename(f)
    d = re.findall("\d+", basename)[0]
    intd = int(d)
    if intd > maxbatch: maxbatch = intd

  # A hack to find the index of the last project that was timed.
  batchnum = maxbatch + batchsize 

  # Determine which projects have already been timed
  already_timed = []
  if os.path.exists(successfile):
    with open(successfile, 'r') as f:
      lines = f.readlines()
      already_timed.extend(lines)

  if os.path.exists(failurefile):
    with open(failurefile, 'r') as f:
      lines = f.readlines()
      already_timed.extend(lines)

  already_timed = [os.path.basename(item) for item in already_timed]
  already_timed = [x.replace("\n", "") for x in already_timed] # remove newlines if present
 
  not_timed = [x for x in projectlist if x not in already_timed]

  if len(not_timed) == 0:
      loggit("All projects timed.")
      print("All projects timed.")
      return

  # The cabal command to do the actual timing
  cmd_time = "{ output=$(TIMEFORMAT='%3R,%3U,%3S' bash -c 'time (cabal new-test -w all --with-compiler=" + compiler + " --ghc-option=-w)') ;} 3>&1" 
  # Only process projects which haven't been processed yet
  # Find the number of projects processed and set the index with it.
  newidx   = maxbatch + 1

  # Determine the indices of the projects in the list to time, based upon
  # how many projects have already been timed.
  # If the timing was interrupted, this should continue where it left off
  # with the remaining projects.
  if (not len(sys.argv) == 2) and (len(projectlist) > batchsize):
  # Create a list of start indices and a list of end indices. For example:
  #   [0, 10, 20]
  #   [9, 19, 29]
  # would create three batches: 0-9, 10-19, 20-29.
    # Max of range is inclusive, so add 1
    endindices   = [x for x in range(batchnum, len(projectlist)) if (x+1) % batchsize == 0]
    startindices = [x for x in range(newidx, len(projectlist)) if x % batchsize == 0] 
    # Add last end index, if projects are not in multiples of the batch number
    lastidx = len(projectlist)-1
    if endindices[-1:][0] != lastidx : endindices.append(lastidx) 

  # A message to indicate timing will start and to notify the user to cease other processes
  # which may confound results.
  print("Sleeping for one minute... please evacuate the machine in a calm and orderly fashion...")
  time.sleep(60)
  print("Nap over.")

  # Version of cabal to specify to avoid warnings
  # (Warnings confuse the timer; it thinks something went
  # wrong and tosses the results.) 
  cabal_update_cmd = "cabal v2-update 'hackage.haskell.org,2020-05-17T21:23:53Z'"
  do_cmd(cabal_update_cmd)

  # Collect times for each batch of projects, as indicated by the
  # start and end indices
  for i in range(0, len(startindices)):
    print("Timing batch with projects " + str(startindices[i]) + " to " + str(endindices[i]))
    batchfile = timesfile + str(endindices[i]) + ".txt" 

    # Time each of the projects
    for projname in projectlist[startindices[i]:endindices[i]]:
        print(projname)
        projectpath = os.path.join(projdir, projname)
        loggit(cmd_time)
        # Ignoring freeze files just to see if 8p10 times things
        print("Timing " + projname + " ...")
        runs = 0
        fail = False
        try:
            os.chdir(projectpath)
            while runs < timings:
                p = subprocess.Popen(cmd_time,
                        shell=True,
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE)
                stdout, stderr = p.communicate()
                outstring = stderr.decode() #str(stderr) 
                outstring = outstring.rstrip()
                times = outstring.split(',')
                r = times[0]
                u = times[1]
                s = times[2]
                for item in [r, u, s]:
                    # Only record times if the output is \d+
                    # e.g., not "warning/error: blah blah"
                    # Don't record garbage printouts from failed timings.
                    # (Not the best regex pattern, but should be good enough.)
                    if not re.match("^(\d|\.)+$", item): 
                        msg = 'Timing output incorrect: ' + str(item)
                        print(msg)
                        raise Exception(msg) 
                rtime = ",".join([projname, "real", str(runs), r])
                utime = ",".join([projname, "user", str(runs), u])
                stime = ",".join([projname, "sys",  str(runs), s])
                # add project times to file
                with open(batchfile, 'a+') as f:
                    f.write(rtime + "\n")
                    f.write(utime + "\n")
                    f.write(stime + "\n")
                runs += 1
        except:
            print("Failure timing " + projname)
            # add project to failures
            with open(failurefile, 'a+') as f:
                f.write(projname + "\n")
            runs = timings # Don't do any more timing; skip project
            fail = True
        os.chdir(expdir)
        if not fail:
            print("Successfully timed " + projname)
            with open(successfile, 'a+') as f:
                f.write(projname + "\n")

# Timing by -project_list has not been tested since refactor
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="""Time Haskell projects and store times in CSV files.""")
    parser.add_argument("project_dir", type=str, help="""The directory containing the projects to be timed.""")
    parser.add_argument("compiler", type=str, help="The compiler to time the projects with. (This must be the same compiler used to build the projects.")
    parser.add_argument("-project_list", type=str, help="""OPTIONAL. A file containing newline-separated project names to be selectively timed from the given folder.""", required=False)
    args = parser.parse_args()
    projdir = args.project_dir
    compiler = args.compiler
    projlist = args.project_list

    time_projects(projdir, compiler, projlist)
