<?php

function gcd ($a, $b)
{
    if ($a == 0) {
        return $b;
    } else {
        return gcd ($b, $a % $b);
    }
}

$a = gcd (24, 8);

check ($a, "[TyInt]");


?>