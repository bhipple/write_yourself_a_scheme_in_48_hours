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

if [ "#t" == "$(./a.out "(eqv? '(1 2 3) '(1 2 3))")" ]; then echo "Test ok"; else
    echo "Test fail"
fi

if [ "a" == "$(./a.out "(car '(a simple test))")" ]; then echo "Test ok"; else
    echo "Test fail"
fi

if [ "(simple test)" == "$(./a.out "(cdr '(a simple test))")" ]; then echo "Test ok"; else
    echo "Test fail"
fi

if [ "#f" == "$(./a.out "(eqv? 2 \"2\")")" ]; then echo "Test ok"; else
    echo "Test fail"
fi

if [ "#t" == "$(./a.out "(equal? 2 \"2\")")" ]; then echo "Test ok"; else
    echo "Test fail"
fi
