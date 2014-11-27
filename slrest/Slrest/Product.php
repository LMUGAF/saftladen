<?php

namespace Slrest;

use Tonic;
use PDO;


class Product extends ResourceBase {
	function __construct(Tonic\Application $app, Tonic\Request $request, array $urlParams) {
		parent::__construct(
			$app,
			$request,
			$urlParams,
			"product",
			array(
				"name"        => array("optional" => true,  "type" => PDO::PARAM_STR),
				"ean"         => array("optional" => false, "type" => PDO::PARAM_STR),
				"price"       => array("optional" => true,  "type" => PDO::PARAM_STR),
				"amount"      => array("optional" => true,  "type" => PDO::PARAM_STR),
				"volume"      => array("optional" => true,  "type" => PDO::PARAM_INT),
				"caffeine"    => array("optional" => true,  "type" => PDO::PARAM_STR),
				"alcohol"     => array("optional" => true,  "type" => PDO::PARAM_INT),
				"note"        => array("optional" => true,  "type" => PDO::PARAM_INT),
				"description" => array("optional" => true,  "type" => PDO::PARAM_INT)
			)
		);
	}
}

