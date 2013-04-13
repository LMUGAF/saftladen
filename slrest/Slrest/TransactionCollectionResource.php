<?php

namespace Slrest;

use Tonic\Response;
use PDO;


/**
 * This class defines an example resource that is wired into the URI /example
 * @uri /transactions
 */
class TransactionCollectionResource extends Transaction {
	/**
	 * @method GET
	 */
	function get() {
		return $this->getAll();
	}
	
	
	/**
	 * @method POST
	 */
	function add() {
		$st = $this->db->prepare("INSERT INTO `transaction` SET ".$this->pdoSet($values));
		if($st->execute($values)) {
			return new Response(Response::CREATED);
		}
		else {
			return new Response(Response::BADREQUEST);
		}
	}
}