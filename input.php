<?php

$a = 1;
while (true) {
    $a[1] = $a;
}

check ($a, "[TyAny]");

?>