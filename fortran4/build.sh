#!/bin/bash
#
#


#HSFLAGS="-Wall -rtsopts -auto-all -caf-all -fobject-code -fforce-recomp -funbox-strict-fields --make"
HSFLAGS="-O2 -threaded -rtsopts -auto-all -caf-all -fobject-code -fforce-recomp -funbox-strict-fields --make"
HSC="ghc"

rm -f *.hi *.hp *.o *.ps *.pdf *.png *.aux *.ps *.eps *.pdf *.png *.prof

HSC_FILES=`ls -1 *.hs`
HSC_OUTP=fortran4

for hsf in `ls -1 *.hs | sort -n`; do
    rm -f "${hsf}.bin"
    hsbin=$(echo $hsf | sed 's/.hs/.bin/g')
    rm -f $hsbin
done

$HSC $HSFLAGS $HSC_FILES -o $HSC_OUTP

rm -f *.hi *.hp *.o *.ps *.pdf *.png *.aux *.ps *.eps *.pdf *.png *.prof
