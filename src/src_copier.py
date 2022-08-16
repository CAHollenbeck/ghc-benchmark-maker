import argparse
import os
import sys
from useful_things import datadir, do_cmd, loggit

# Usage: python3 src_copier.py <source_dir> <destination_dir> [<project_file>]
#
# This script copies projects from the source folder into the destination folder.
# Optionally, a file can be passed which indicates which subset of projects to
# copy from the source folder.

def copy_projects(sourcedir, destdir, listfile=None):
  # get last folder name in destination path to help name the file
  # that will contain successful builds.
  tmp = os.path.normpath(destdir)
  destbase = os.path.basename(tmp)
  #projectfile = os.path.join(datadir, "build_successes_projects.txt")
  if listfile:
    with open(listfile) as f: projects = f.read().splitlines()
  else:
    projects = os.listdir(sourcedir)

  # Create the destination folder, if it doesn't already exist
  if not os.path.exists(destdir):
    mkdir = "mkdir " + destdir
    do_cmd(mkdir)

  for proj_ in projects:
    proj = proj_.split(",")
    proj = proj[0]
    try:
      # Copy the projects into the nopragma directory
      copy_cmd = ""
      sourcepath = os.path.join(sourcedir, proj)
      destpath = os.path.join(destdir, proj)
      # Just copy the project over, even if it already exists. Overwrite it.
      copy_cmd = "cp -rf " + sourcepath + " " + destpath
      do_cmd(copy_cmd)
    except Exception as e:
        msg = "src_copier exception: " + str(e)
        loggit(msg)
        print(msg)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Copy projects from source folder into destination folder. Optionally pass a file containing project names to select from the source folder and exclude projects not in the list.")
    parser.add_argument("source", type=str, help="The directory of projects to be copied.")
    parser.add_argument("destination", type=str, help="The desired name of the directory to where projects will be copied.")
    parser.add_argument("-list", type=str, help="A newline-separated file containing names of projects to be copied from source folder to destination folder.", required=False, default=None)
    args = parser.parse_args()
    srcdir = args.source
    dest = args.destination
    listfile = args.list
    copy_projects(srcdir, dest, listfile)
