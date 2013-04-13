<?php

namespace Slrest;
use Tonic;
use PDO;


class User extends ResourceBase {
	function __construct(Tonic\Application $app, Tonic\Request $request, array $urlParams) {
		parent::__construct(
			$app,
			$request,
			$urlParams,
			"user",
			array(
				"name"    => array("optional" => false, "type" => PDO::PARAM_STR),
				"mail"    => array("optional" => true,  "type" => PDO::PARAM_STR),
				"gaf_acc" => array("optional" => true,  "type" => PDO::PARAM_STR),
				"irc"     => array("optional" => true,  "type" => PDO::PARAM_STR),
				"total"   => array("optional" => true,  "type" => PDO::PARAM_INT),
				"note"    => array("optional" => true,  "type" => PDO::PARAM_STR),
				"enabled" => array("optional" => true,  "type" => PDO::PARAM_INT)
			)
		);
	}
}

