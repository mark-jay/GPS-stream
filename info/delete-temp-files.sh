find . -type f -name "*#"   -exec rm -f {} \;
find . -type f -name "*~"   -exec rm -f {} \;
find . -type f -name "*.hi" -exec rm -f {} \;
find . -type f -name "*.o"  -exec rm -f {} \;
