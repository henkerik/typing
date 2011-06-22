<?php

$a = 1;
$b[1] = $a;
$c = $b[1];

check ($c, "[TyInt]");

?>