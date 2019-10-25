import urllib.request
import os
import shutil
import sys


# the tag of the version the API which you want to generate
api_version = "gmsh_4_4_1"

base = f"https://gitlab.onelab.info/gmsh/gmsh/raw/{api_version}/"

files = ["CMakeLists.txt",
         "api/gen.py"]

# download necessary files from GMSH repository
for f in files:
    url = base + f
    cont = urllib.request.urlopen(url).read()
    with open(f, 'wb') as fhandle:
        fhandle.write(cont)

# run gen.py, which should run GenAPI.py which should generate
# src/GmshAPI.hs.
sys.path.insert(0,'api')
import gen
