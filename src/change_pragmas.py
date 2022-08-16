import argparse
import os
import subprocess
import sys
from useful_things import datadir, do_cmd, loggit, logerr

# This script replaces OLDPRAGMA with NEWPRAGMA in the
# src folders of projects in the directory provided.
# For example:
# change_pragmas.py INLINE REMOVEINLINE FOLDER ->
# {-# INLNE ... -> {-# REMOVEINLINE ...

# Make sure the pragma provided is valid
def pragma_validate(p):
    r = "REMOVE"
    i = "INLINE"
    n = "NOINLINE"
    ea = "INLINEABLE"
    a  = "INLINABLE"
    if p in [ i, n, ea, a,
              r+i, r+n, r+ea, r+a]:
        return p
    else:
        msg = "Invalid pragma: " + p
        print(msg)
        logerr(msg)
        sys.exit()

# Substitute the pragmas in the given folder with the given string.
#
# projdir : the directory containing projects where pragmas should be changed
# frompragma : the pragma to change
# topragma : the string to substitute the pragma with
def change(frompragma, topragma, projdir):
  projects = os.listdir(projdir)
  #breakpoint()

  for proj in projects:
    try:
      # Replace the pragmas in-place
      projsrc = os.path.join(projdir, proj, "src/")
      # The sed command needs '' in the commented line below to work on Mac
      #change_inlines = "find " + projsrc + " -type f -print0 | xargs -0 sed -i '' 's/{-# " + frompragma + " /{-# " + topragma + " /g'"
      # Use the sed below for linux
      change_inlines = "find " + projsrc + " -type f -print0 | xargs -0 sed -i 's/{-# " + frompragma + " /{-# " + topragma + " /g'"
      do_cmd(change_inlines)
    except Exception as e:
      loggit("except: " + str(e))
      loggit("Unable to change pragmas in: " + proj)    

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Change pragmas in projects' src folders. Options include: INLINE, NOINLINE, INLINEABLE, INLINABLE, REMOVEINLINE, REMOVENOINLINE, REMOVEINLINEABLE, REMOVEINLINABLE. Prepending a pragma with REMOVE causes the compiler to ignore it.")
    parser.add_argument("from_pragma", type=str, help="The pragma to be changed.")
    parser.add_argument("to_pragma", type=str, help="The value the pragma should be changed to.")
    parser.add_argument("directory", type=str, help="The directory containing the projects where the pragmas should be changed.")
    args = parser.parse_args()
    frompragma = args.from_pragma
    topragma = args.to_pragma
    pragma_validate(frompragma)
    pragma_validate(topragma)
    projdir = args.directory
    change(frompragma, topragma, projdir)
