#!/bin/bash
# recursively removes all files that do not have a .hs or .sh extension 
find . -type f  ! \( -name "*.hs" -o -name "*.sh" \) -delete