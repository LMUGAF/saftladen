<?php

namespace Slrest;
use Tonic;
use PDO;


class Transaction extends ResourceBase {
	function __construct(Tonic\Application $app, Tonic\Request $request, array $urlParams) {
		parent::__construct(
			$app,
			$request,
			$urlParams,
			"transaction",
			array(
				"product_id"     => array("optional" => true,  "type" => PDO::PARAM_STR),
				"user_id"        => array("optional" => false, "type" => PDO::PARAM_STR),
				"transaction_id" => array("optional" => true,  "type" => PDO::PARAM_STR),
				"type"           => array("optional" => true,  "type" => PDO::PARAM_STR),
				"price"          => array("optional" => true,  "type" => PDO::PARAM_INT), // Only when not product_id is set
//				"date"           => array("optional" => true,  "type" => PDO::PARAM_STR),
				"origin"         => array("optional" => true,  "type" => PDO::PARAM_INT)
			)
		);
	}
}

