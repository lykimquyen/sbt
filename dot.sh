for f in *.dot; do echo ---------------------$f--------------->> $f.ps; ~/local/bin/dot -Tps $f -o 2>&1 | cat >> $f.ps;done
