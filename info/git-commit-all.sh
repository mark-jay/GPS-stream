cd /home/fallen/projects/haskell/GPS-stream/
rm Main
rm log.log
rm dmp.txt
find . -type f -name "*#" -exec rm -f {} \;
find . -type f -name "*~" -exec rm -f {} \;
find . -type f -name "*.o" -exec rm -f {} \;
find . -type f -name "*.hi" -exec rm -f {} \;
git add --all
git commit -m 'commit'
git push origin master
