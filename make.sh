for f in  *.ka; do echo ---------------------$f--------------->> log.txt;
~/local/KaSim2/bin/KaSa $f 2>&1 | cat >> log.txt; done




