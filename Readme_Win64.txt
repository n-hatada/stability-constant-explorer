========================================================
Stability Constants Explorer For Windows 64bit
v1.0, released on May 25, 2022
https://github.com/n-hatada/stability-constants-explorer
========================================================
## Overview
Stability Constants Explorer is a simple search program for [NIST SRD 46 database, "Critically Selected Stability Constants of Metal Complexes"](https://data.nist.gov/od/id/mds2-2154). This database is very useful for chemists, but unfortunately it has been discontinued and the official search program does not work on modern PCs. Therefore, I have created a simple search program based on the data available on the NIST website.

## Download
For Microsoft Windows 7 or later (64 bit), download [StabilityConstantsExplorer_Win64.zip](https://github.com/n-hatada/stability-constants-explorer/releases/download/v1.0/StabilityConstantsExplorer_Win64.zip).

## Install
### On Microsoft Windows 7 or later (64 bit)
Unzip the downloaded compressed file to an appropriate folder. The compressed file contains the following files:
* Executable program (StabilityConstantsExplorer.exe)
* SQLite3 database engine (sqlite3.dll)
* Stability constants database (NIST_SRD_46_ported.db)
* Documentation (Readme_Win64.txt)

## Usage
Execute the program. On the left side of the window, specify the search criteria for stability constants (metal ions and ligands). The search results are displayed on the right side.

## Author
Naoyuki Hatada, Ph.D.
Department of Materials Science and Engineering, Kyoto University

## Data source
The accompanying database file (NIST_SRD_46_ported.db) is based on the following dataset which is distributed at the NIST website:

* Donald R. Burgess (2004), NIST SRD 46. Critically Selected Stability Constants of Metal Complexes: Version 8.0 for Windows, National Institute of Standards and Technology, https://doi.org/10.18434/M32154 (Accessed June 25, 2021)

The author (Naoyuki Hatada) downloaded the dataset (SRD 46 SQL.zip) from the above website and converted to a database file in the SQLite3 format (NIST_SRD_46_ported.db). The database file may contain errors described in "SRD 46 README.txt" on the above website or those caused by the conversion process.

## License
The database file (NIST_SRD_46_ported.db) is a derivative work of the data distributed by NIST. See the [NIST website](https://doi.org/10.18434/M32154) for usage guidelines. In particular, you must follow the text quoted below from "SRD 46 README.txt" on the NIST website.

>You may improve, modify, and create derivative works of the data or any portion of the data, and you may copy and distribute such modifications or works. Modified works should carry a notice stating that you changed the data and should note the date and nature of any such change. Please explicitly acknowledge the National Institute of Standards and Technology as the source of the data:  Data citation recommendations are provided at https://www.nist.gov/open/license.

The SQLite3 engine (sqlite3.dll) is in the public domain. The other files (source code and the executable program) are released into the public domain by the author.