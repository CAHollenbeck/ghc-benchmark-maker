import os
import sys
from useful_things import do_cmd

def copy_to(path):
  # Get the list of projects by just listdir project file
  projects = os.listdir(path)
  cp_cabal = "cp ../MODIFIED_DEPENDENCIES/cabal.project "
  
  # For each project
  for p in projects:
    projpath = os.path.join(path, p)
    cp_cabal_p = cp_cabal + projpath
    do_cmd(cp_cabal_p) # Copy the cabal.project file

if __name__ == "__main__":
    path = sys.argv[1]
    copy_to(path)
