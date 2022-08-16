import argparse
import change_pragmas
import download_and_build
import os
import project_timer
import src_copier
import subprocess
import sys
from useful_things import datadir, do_cmd, expdir, logdir, loggit, origdir, running

#########################################################################
#
# This script calls all of the smaller scripts to download
# the projects from a Stackage Nightly snapshot, unzip them,
# build them, and time them.
#
# This script also specifically copies the projects to a MODIFIED
# folder and removes INLINE pragmas, then times those projects as well.
#
# Usage: python3 do_everything.py <project-list> <compiler> 
#
# File   project-list  : A file containing Haskell package names. Each
#                        package name should include the version number
#                        and be separated by a newline.
# String compiler      : Full path to the GHC compiler to build and run
#                        tests.
#
# CAUTION: This script works by removing and resetting Cabal for every
# run of the benchmarks. This was necessary because a memory leak, we 
# think, causes run times to linearly increase. We were not able to 
# investigate why.
#
#########################################################################


# Parse command line arguments
parser = argparse.ArgumentParser(description="Download Haskell projects, build & time them.")
parser.add_argument("project_file", type=str, help="File of Haskell project names & versions, separated by newlines.")
parser.add_argument("compiler", type=str, help="Compiler (location) for testing baseline.")
args = parser.parse_args()
project_file = args.project_file
compiler = args.compiler

moddir = os.path.join(expdir, "MODIFIED")

# Create the experiments directory to place everything in
if not os.path.exists(expdir):
    os.mkdir(expdir)

# Create the log directory if it doesn't exist yet.
if not os.path.exists(logdir):
    os.mkdir(logdir)
    loggit("Input file: " + project_file)

# Create the data directory if it doesn't exist yet.
if not os.path.exists(datadir):
    os.mkdir(datadir)

print("Downloading and building projects...")

# Command to call the script to download and build the original projects
successfile = download_and_build.download_and_build(project_file, compiler)
 
print("Copying projects to MODIFIED folder...")

# Copy successfully built projects to the MODIFIED folder
src_copier.copy_projects(origdir, moddir, successfile)

print("Removing INLINE pragmas...")

# Change INLINE pragmas to REMOVEINLINE pragmas
# "REMOVEINLINE" is not a valid pragma name, so GHC will just skip over them.
change_pragmas.change("INLINE", "REMOVEINLINE", moddir)

print("Timing projects in: " + origdir)
# Time the original, ummodified projects
project_timer.time_projects(origdir, compiler)
 
# Time the modified projects
print("Timing projects in: " + moddir)
project_timer.time_projects(moddir, compiler)
