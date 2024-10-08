# Stability Constant Explorer
## Overview
Stability Constant Explorer is a simple search program for [NIST SRD 46 database, "Critically Selected Stability Constants of Metal Complexes"](https://data.nist.gov/od/id/mds2-2154). This database is very useful for chemists, but unfortunately it has been discontinued and the official search program does not work on modern PCs. Therefore, I have created a simple search program based on the data available on the NIST website.

## Download
* For Microsoft Windows 7 or later (64 bit), download [StabilityConstantExplorer_Win64.zip](https://github.com/n-hatada/stability-constant-explorer/releases/download/v1.1.2/StabilityConstantExplorer_Win64.zip) (v1.1.2, Released on Sep. 25, 2023.).
* The Source code is available on [GitHub](https://github.com/n-hatada/stability-constant-explorer).

## Install

### On Microsoft Windows 7 or later (64 bit)
Unzip the downloaded compressed file to an appropriate folder. The compressed file contains the following files:
* Executable program (StabilityConstantExplorer.exe)
* SQLite3 database engine (sqlite3.dll)
* Stability constants database (NIST_SRD_46_ported.db)
* Documentation (Readme_Win64.txt)

### On other operating systems
Compile the source code using [Lazarus](https://www.lazarus-ide.org/). Obtain and install an appropriate [SQLite3](https://www.sqlite.org/index.html) engine. Place the database file (NIST_SRD_46_ported.db) in the same folder as the compiled program.

## Usage
Execute the program. On the left side of the window, specify the search criteria for stability constants (metal ions and ligands). The search results are displayed on the right side.
![Screenshot](assets/screenshot.png)

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

The SQLite3 engine (sqlite3.dll) is in the public domain. The other files (the source code and the executable program) are released into the public domain by the author. 

## Version history
### v1.0, released on May 27, 2022
* Initial release.
### v1.0.3, released on Oct. 26, 2022
* Bug fix.
### v1.1.0, released on Mar. 24, 2023
* Metal ions and ligands can be filtered by text.
* Value types can be specified in the search criteria.
* Minor improvements. (e.g., superscript and subscript characters are shown in the metal ion list, and the lists are made read-only.)
### v1.1.1, released on Apr. 6, 2023
* Minor fixes and improvements.
* Update SQLite3 engine.
### v1.1.2, released on Sep. 25, 2023
* Bug fix. (Apostrophe characters in ligand names have caused crashes.) 