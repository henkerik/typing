<?php

function foo ()
{
    if (true) {
        return true;
    } else {
        return 1;
    }
}

$a = foo ();

check ($a, "[TyInt,TyBool]");

?>