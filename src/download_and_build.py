import argparse
import copy_deps
import find_pragmas
import glob
import os
import project_builder
import subprocess
import sys
from useful_things import logdir, logerr, loggit, do_cmd, running, origdir

############################################################################
#
# Usage: python3 download_and_build.py <project-list-file> <compiler>
#
# This script calls the scripts to download the tar files,
# unzip the tar files,
# copy QuickCheck dependency files,
# copy the freeze files to the projects folders,
# build the projects.
#
# There is also commented-out code to create the freeze files, 
# assuming the projects have been downloaded and built for the first
# time.
#
# File   project-list-file  : A file containing Haskell package names. 
#                        Each package name should include the version
#                        number and be separated by a newline.
# String compiler      : Full path to the GHC compiler to build and run
#                        tests.
#
############################################################################

def download_tars(fname):
  if not os.path.exists(origdir):
    os.mkdir(origdir)

  projlist = [line.rstrip('\n') for line in open(fname)]  
  # Download the tar files for each project. Log whether download was successful
  for proj in projlist:
    zipfile = proj + ".tar.gz"
    # Curl the project from Hackage
    downloadurl = "http://hackage.haskell.org/package/" + proj + "/" + zipfile
    cmd = "curl " + downloadurl + " -o " + os.path.join(origdir, zipfile)
    do_cmd(cmd)

def unzip(pdir, tars):
  # Unzip and then remove each .gz file
  for t in tars:
    cmd_unzip = "tar -xvf " + t + " --directory " + pdir
    p_unzip = do_cmd(cmd_unzip)
        
    cmd_cleanup = "rm " + t
    p_cleanup = do_cmd(cmd_cleanup)

def start_unzip(pdir):
    zippedfiles = glob.glob(os.path.join(pdir, "*gz"))

    # Unzip files if tars exist.
    if zippedfiles:
      unzip(pdir, zippedfiles)
    else: # If no .gz files in the project directory, then done
      print("All projects unzipped.")
      loggit("All projects unzipped.")

def download_and_build(project_file, compiler):
  # Download tar files
  download_tars(project_file)

  # Unzip tar files
  start_unzip(origdir)

  # FINDING THE PRAGMAS
  # does nothing if pragmas already found
  find_pragmas.find_pragmas(origdir)

  # Copy cabal.project from modified dependencies folder
  copy_deps.copy_to(origdir)

  # BUILDING THE PROJECTS
  successfile = project_builder.build_projects(origdir, compiler)
  return(successfile)

if __name__ == "__main__":
  parser = argparse.ArgumentParser(description="""Download & unzip Haskell projects' tar 
          files, substitute deterministic QuickCheck, freeze projects, build projects.""")
  parser.add_argument("project_file", type=str, help="""File of Haskell project names & 
          versions, separated by newlines.""")
  parser.add_argument("compiler", type=str, help="Compiler (location) for testing baseline.")
  args = parser.parse_args()
  project_list = args.project_file
  compiler = args.compiler

  download_and_build(project_list, compiler)
