import glob
import os
import pickle
import re
import subprocess
import sys
from useful_things import datadir, do_cmd, loggit, origdir, tempdir


# Usage: python3 find_pragmas.py
#
# Read in file containing project names
# Count number of lines
# Divide into batches of size N
# process batches

# An object that holds pragma counts for a project
class PragmaProject:
  def __init__(self, projectname, inlines, noinlines, inlinables):
    self.projectname = projectname
    self.inlines     = inlines
    self.noinlines   = noinlines
    self.inlinables  = inlinables

def find_pragmas(projdir):
  batchdir = os.path.join(datadir, "BATCHES")
  projlist = glob.glob(os.path.join(projdir, "*"))
  batchsize = 300
  idx_low = 0
  idx_high = batchsize
  numproj = len(projlist)
  lows = []
  highs = []
  projnames_w_pragmas = []
  projnames_w_inline_pragmas = []
  projs_w_pragmas = []

  if not os.path.exists(datadir):
    os.mkdir(datadir)
  if not os.path.exists(batchdir):
    os.mkdir(batchdir)

  def rm_temp():
    rm_temp = "rm -rf " + tempdir
    do_cmd(rm_temp)

  def make_temp():
    mk_temp = "mkdir " + tempdir
    mktemp = do_cmd(mk_temp)

  # Recursively collect Haskell files in the src directory
  def find_haskell_files(pfolder):
    dirs = [d for d in os.listdir(pfolder) if os.path.isdir(os.path.join(pfolder, d))]
    haskellfiles = []
  
    inline    = 0
    noinline  = 0
    inlinable = 0
  
    if 'src' in dirs:
      haskellfiles = find_files_helper(os.path.join(pfolder, 'src'))

    for hf in haskellfiles:
      counts = count_pragmas(hf)
      inline    += counts[0]
      noinline  += counts[1]
      inlinable += counts[2]

    if inline != 0:
      projname = os.path.basename(pfolder)
      projnames_w_inline_pragmas.append(projname)
  
    if (inline + noinline + inlinable) != 0:
      projname = os.path.basename(pfolder)
      proj_obj = PragmaProject(projname, inline, noinline, inlinable)
      projnames_w_pragmas.append(projname)
      return(proj_obj)   
  
  # Recursively collect haskell files in given directory
  def find_files_helper(path):
    haskellfiles = [os.path.join(path, f) for f in os.listdir(path) if (os.path.isfile(os.path.join(path, f)) and is_haskell(f))]
    dirs = [d for d in os.listdir(path) if os.path.isdir(os.path.join(path, d))]
    for d in dirs:
      haskellfiles = haskellfiles + find_files_helper(os.path.join(path, d))
    return(haskellfiles)
  
  
  # Check if a file is a Haskell file
  def is_haskell(filename):
    # Check if the last N letters of a filename indicate a
    # Haskell extension
    if filename[-2:] == 'hs':
      return((filename[-3:] == '.hs') or (filename[-4:] == '.lhs'))
  
  # Count INLINE pragmas in one file
  # return a list of length 3:
  #  return[1] is the inline pragmas
  #  return[2] is the noinline pragmas
  #  return[3] is the inlinable pragmas
  def count_pragmas(filepath):
    inline = 0
    noinline = 0
    inlinable = 0
    try:
      for line in open(filepath):
        if may_have_pragma(line):
          ptype = pragma_type(line)
          if ptype == 1: inline += 1
          if ptype == 2: noinline += 1
          if ptype == 3: inlinable += 1
    except Exception as e:
      # If the file doesn't open, report no pragmas
      return([0, 0, 0])
    return([inline, noinline, inlinable])    
  
  # may_have_pragma :: Line -> Int
  # Return 1 if the line may have a pragma, 0 otherwise
  def may_have_pragma(line):
    if re.search("INLIN", line):
      return True
    else: return(False)
  
  # pragma_type :: String -> Int
  # return an Int representing the type of pragma
  def pragma_type(line):
    if re.search("\s*{-# INLINE ", line):
      return(1) # 1 for INLINE
    if re.search("\s*{-# NOINLINE ", line):
      return(2) # 2 for NOINLINE
    if re.search("\s*{-# INLINABLE ", line):
      return(3) # 3 for INLINABLE
  
  def toabs(pf):
    if not isabs(pf):
      projects_folder = os.path.join(expdir, pf)
      return(projects_folder)
    else: return(pf)
  
  def start_find(projects_folder, start_index, end_index):
    root_dirs = [os.path.join(projects_folder, d) for d in os.listdir(projects_folder)]
    dirs_to_process = root_dirs[start_index:end_index]
  
    for pdir in dirs_to_process:
      pobj = find_haskell_files(pdir)
      if pobj is not None:
        projs_w_pragmas.append(pobj)
  
    outfile = "pragma_data_" + str(start_index) + "_" + str(end_index) + ".pkl"
    with open(os.path.join(datadir, outfile), 'wb') as f:
      pickle.dump(projs_w_pragmas, f)


    # Save names of projects with pragmas
    outfile_names = "projects_with_pragmas.txt"
    with open(os.path.join(datadir, outfile_names), 'a+') as f:
      for pname in projnames_w_pragmas:
         f.write(pname + "\n") 

    inline_outfile = "projects_with_inline_pragmas.txt"
    with open(os.path.join(datadir, inline_outfile), 'a+') as f:
      for pname in projnames_w_inline_pragmas:
          f.write(pname + "\n")

  # Create arrays containing the lower and upper indices 
  # to break up the project list into batches
  while idx_low < numproj:
    if idx_high > numproj:
        idx_high = numproj
    lows.append(idx_low)
    highs.append(idx_high)
    
    idx_low += batchsize
    idx_high += batchsize

  idxlen = len(lows)

  # Write project names for each batch into files
  for i in range(0, idxlen):
      # Write each batch list to file
      batchname = "batch_" + str(lows[i]) + "_" + str(highs[i]) + ".txt"
      batchfilepath = os.path.join(batchdir, batchname)
      if not os.path.exists(batchfilepath):
          with open(batchfilepath, 'a+') as f:
              for proj in projlist[lows[i]:highs[i]]:
                  f.write(proj + "\n")

  for i in range(0, idxlen):
    low = str(lows[i])
    high = str(highs[i])
    start_find(origdir, lows[i], highs[i])

if __name__ == "__main__":
  projdir = sys.argv[1]
  find_pragmas(projdir)
