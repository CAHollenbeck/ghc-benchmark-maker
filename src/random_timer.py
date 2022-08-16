from useful_things import expdir, logerr, loggit, do_cmd
from os import killpg, listdir, getcwd, chdir, mkdir
from os.path import join, basename, exists
import argparse
import numpy as np
import psutil
import re
import subprocess
import sys
import signal

#######################################################################
#
#  This file randomly selects numeric values and passes them in to the
#  custom inlining threshold dynamic flags in the modified GHC to 
#  alter the inlining heuristc.
#
#######################################################################


# Version of cabal to specify to avoid warnings
# (Warnings confuse the timer; it thinks something went
# wrong and tosses the results.) 
cabal_update_cmd = "cabal v2-update 'hackage.haskell.org,2020-05-17T21:23:53Z'"

# timeThem :: String -> String -> ...OptionalArgs... -> IO Void
#
# This function times cabal test on projects residing in the
# passed-in folder(s) and writes the test timings to a CSV file.
# Threshold flags may optionally be passed to set them to 
# particular values; however, the function produces random 
# values for them by default from normal distributions.
#
# [String] compiler : The desired binary of the GHC to time with. 
# [String] projectdirs : A list of the project directories 
#                  containing projects to time.
# [String, Int] uoe_ : A threshold. The string is the threshold
#                  flag for the GHC, and the int is its default
#                  value.
# Int    n_times : The number of times to run each configuration.
# Int    n_configurations : The number of random configurations of
#                  thresholds to try per batch.
# String cabal_update : the 'cabal update' to update the package
#                  index. Cabal emits warnings without this
#                  command, which confuse the timer--so it
#                  disregards the timings and throws them away.
# Int    to : the timeout for the compilation
def timeThem(compiler,
             projectdirs,
             uoeFI=["-thresh-funcitself-disc", 10],
             uoeAA=["-thresh-actarg-disc", 10],
             uoeNTA=["-thresh-nontrivarg-disc", 10],
             uoeDAC=["-thresh-discargctxt-disc", 40],
             uoeRAC=["-thresh-ruleargctxt-disc", 40],
             uoeRC=["-thresh-rhsctxt-disc", 40],
             uoeAC=["-thresh-arbctxt-disc", 40],
             ffd=["-funfolding-fun-discount", 60],
             fdd=["-funfolding-dict-discount", 30],
             uoeCOB=["-thresh-cosbase", 20],
             uoeCOA=["-thresh-cosargs", 10],
             uoeBA=["-thresh-bigalt", 20],
             uoeCnS=["-thresh-consize", 10],
             uoeLS=["-thresh-litstr", 10],
             uoeUI=["-thresh-uncinl", 10],
             n_times=6,
             n_configurations=1,
             cabal_update=cabal_update_cmd,
             time_timeout=7200, # Seconds to time out for test 
             build_timeout=10800, # Seconds on time out for build 
             distn="none"):

  cwd = getcwd() # Get current directory as cwd

  # Just the command 'cabal new-clean'
  # save time to not execute this.
  cmd_clean = "cabal new-clean"

  # Command to rebuild the project with the given compiler.
  cmd_build = "cabal new-build all --with-compiler=" + compiler

  # Location of the timing CSV file
  time_dir = join(expdir, "RANDOM_TIMES")
 
  if not exists(expdir):
      mkdir(expdir)

  if not exists(time_dir):
      mkdir(time_dir)
      
  time_file = join(time_dir,  "random_times.csv")

  # For reuse
  ghcopt = "--ghc-option="

  # Clear out the cabal library
  cmd_cabal_rm = "rm -rf ~/.cabal"

  # getUnif :: Int -> Int -> Int
  #
  # lb : lower bound
  # ub : upper bound
  #
  # Get a randomly generated Int
  # from a uniform distribution,
  # given the lower and upper bounds.
  def getUnif(lb, ub):
    n = np.random.randint(lb, ub)
    return(n)

  # getNorm :: Int -> Int -> Int
  #
  # mu : mean
  # s : standard deviation
  #
  # Get a randomly generated integer from
  # a provided mean and standard deviation.
  def getNorm(mu, s):
    n = int(np.random.normal(mu, s))
    if n >= 0:
      return n
    else:
      return getNorm(mu, s)

  # Repeat this below for N number of configurations. This creates
  # the random thresholds, writes them into a 'cabal time' string
  # as passed-in ghc options, and executes it for the number of 
  # timings specified--writing the results to a CSV.
  for c in range(0, n_configurations):
    print("c: " + str(c))
    print("configurations: " + str(n_configurations))
    print("n_times: " + str(n_times))
    thresholds = []
    threshnums = []

    # A list of the thresholds to generate random values for.
    # Remove items from this list to keep them at their defaults. 
    flags_tochange = [uoeFI,
                      uoeAA,
                      uoeNTA,
                      uoeDAC,
                      uoeRAC,
                      uoeRC,
                      uoeAC,
                      ffd,
                      fdd,
                      uoeCOB,
                      uoeCOA,
                      uoeBA,
                      uoeCnS,
                      uoeLS,
                      uoeUI
                      ]

    # If we're doing the threshold-parameter GHC, assign the random
    # thresholds.
    for t in flags_tochange:
        n = t[1] # the flag's default value
        v = ghcopt + t[0] + "="
        if distn=="norm":
            # Get values from normal distributions for each flag,
            # determined by each flag's default value.
            #  
            # The means and std deviations are hand-coded here:
            # getNorm(mean, stddev).

            std_dev = int(n * .4)
            n = getNorm(mean, std_dev)
        elif distn=="unif":
            lowbound = 0
            highbound = 2 * n
            n = getUnif(lowbound, highbound)

        # For each threshold, write threshold=value to a string to
        # be appended to the time command.
        v = v + str(n)
        thresholds.append(v)
        threshnums.append(str(n))

    # To build the project with the given compiler
    cmd_build = "cabal new-build all --with-compiler=" + \
               compiler + " " + " ".join(thresholds) +  " " + ghcopt + "-w" 
    
    # For composing the string for the command to time each
    # project
    endstring = ")') ;} 3>&1"
    cmd_time = "{ output=$(TIMEFORMAT='%3R,%3U,%3S'" + \
               " bash -c 'time (cabal new-test -w all --with-compiler=" + \
               compiler + " " + " ".join(thresholds) +  " " + ghcopt + "-w" + endstring 

    # Clear out the cabal library before building the projects
    do_cmd(cmd_cabal_rm)

    # cabal update to prevent warnings
    do_cmd(cabal_update)

    # List of project names to time
    projects = []

    # List the contents of the passed-in projects directory/ies
    # to get names of projects to time.
    for pd in projectdirs:
      project_list =  listdir(pd)

      projects += [join(cwd, pd, p) for p in project_list]

    projects.sort() # Sort the list alphabetically. I renamed poly as aaapoly
    # because I want to time it first. That one takes the longest to compile,
    # and if it times out, we want to skip the whole configuration

    # Set this to true if a package times out during compilation
    # We don't want to spend ages compiling packages because of unfeasible
    # threshold configurations.
    stop = False


    # Go through each project in the project list and time it.
    for proj in projects:
      proj_base = basename(proj)

      if stop == False:
          # cd into project directory
          print("TIMING: " + proj)
          print(proj_base)
          chdir(proj)
          count = 1

          
          # Do the initial compile. Set it at an N-second timeout.
          # The first run won't be recorded in the CSV because it will
          # include some compile time.
          print(cmd_build)
          with subprocess.Popen(cmd_build, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE) as process:
              try:
                  print("Trying to build " + proj)
                  output = process.communicate(timeout=time_timeout)[0]
                  #output = process.communicate(timeout=60)[0] # shorter timeout for debug
              except subprocess.TimeoutExpired:
                  process.kill() # Kill the process
                  stop = True    # Flag to abandon this configuration
                  break          # Exit this iteration

          print(cmd_time)
          with subprocess.Popen(cmd_time, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE) as process:
              try:
                  print("Trying to time " + proj)
                  output = process.communicate(timeout=build_timeout)[0]
              except subprocess.TimeoutExpired:
                  process.kill() # Kill the process
                  stop = True    # Flag to abandon this configuration
                  break          # Exit this iteration

          # Don't continue timing if compilation exceeded timeout
          if stop == True:
              print("Project timed out. Abandon configuration.")
              break
         
          # Continue timing the configuration the desired number of times.
          while count <= n_times:
            print("count: " + str(count))
            try:
              # Subprocess to run the 'cabal time ...' command
              p = subprocess.Popen(cmd_time,
                                 shell=True,
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE)
              stdout, stderr = p.communicate()
              outstring = stderr.decode() 
              outstring = outstring.rstrip()
              times = outstring.split(',')
              r = times[0] # real time
              u = times[1] # user time
              s = times[2] # sys time

              # Take temperature at time of recording
              temps = psutil.sensors_temperatures()
              tempstring = ""
              # Write temps to a string to append to CSV
              for name, entries in temps.items():
                for entry in entries:
                  tempstring += ("," + str(entry.current))

              # Pull out the real, sys, & user times.
              #
              # Check each in [real, sys, user] times:
              # Only record the time if the output is \d+
              # e.g., not "warning/error: blah blah"
              # Don't record garbage printouts from failed timings.
              for item in [r, u, s]:
                # (Not the best regex pattern, but should be good enough.)
                if not re.match("^(\d|\.)+$", item): 
                  msg = 'Timing output incorrect: ' + str(item)
                  logerr(msg)
                  raise Exception(msg)

              # Format the real, user, and sys times.
              # Each goes into its own row in the CSV
              rtime = (",".join([proj_base,
                                "real", 
                                r,
                                str(c),
                                str(count), 
                                ",".join(threshnums)
                                ]) + tempstring)
              utime = (",".join([proj_base, 
                                 "user", 
                                 u,
                                 str(c),
                                 str(count), 
                                 ",".join(threshnums)
                                 ]) + tempstring)
              stime = (",".join([proj_base, 
                                 "sys",
                                 s,
                                 str(c),
                                 str(count), 
                                 ",".join(threshnums)
                                 ]) + tempstring)

              # Write the timings to the time file
              with open(time_file, 'a+') as f:
                f.write(rtime + "\n")
                f.write(utime + "\n")
                f.write(stime + "\n")
            # If anything goes wrong trying to time a project, print
            # and log the error, then proceed to the next project.
            except Exception as e:
              msg = "Failure or problem on: " + proj + "\n" + str(e)
              print(msg)
              logerr(msg)
            finally:
              count += 1 # Increment the number of timings done
      chdir(cwd) # Change back to cwd


if __name__ == "__main__":
  parser = argparse.ArgumentParser(description="Tool to produce random numbers for the modified GHC, build projets from a specified folder, and collect execution times from those projects.")
  parser.add_argument("compiler", type=str, help="Path to the magic number GHC")
  parser.add_argument("folder", type=str, help="Location of packages to be built and timed")
  parser.add_argument("-configs", type=int, help="The number of random configurations to produce", required=False, default=320)
  parser.add_argument("-timeout", type=int, help="Timeout (seconds) limit to allow GHC to build projects before discarding configuration",required=False, default=120)
  # These are just included as an example of how to call the random timer.
  args = parser.parse_args()
  timeout = args.timeout # How long to let projects compile before timing out
  compiler = args.compiler
  folder = args.folder
  n_configs = args.configs # The number of randomized configurations to try

  # Call the timer
  # The parameters of this call can be changed.
  timeThem(compiler,
           [folder],
           n_times=6,
           n_configurations=n_configs,
           time_timeout=timeout,
           distn="unif")
