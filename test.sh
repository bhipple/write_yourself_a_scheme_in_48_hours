#!/bin/bash
# Really crude testing mechanism

if [ 24 -eq "$(./a.out '(+ 2 (* 3 4) 5 (- 12 7))')" ]; then echo "Test ok"; else
    echo "Test fail"
fi

if [ "#f" == "$(./a.out '(> 2 3)')" ]; then echo "Test ok"; else
    echo "Test fail"
fi

if [ "1" == "$(./a.out '(if (> 3 2) 1 0)')" ]; then echo "Test ok"; else
    echo "Test fail"
fi

if [ "0" == "$(./a.out '(if (> 3 4) 1 0)')" ]; then echo "Test ok"; else
    echo "Test fail"
fi

if [ "1" == "$(./a.out '(eqv (1 2 3) (1 2 3) 1 0)')" ]; then echo "Test ok"; else
    echo "Test fail"
fi
