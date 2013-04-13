<?php

namespace Slrest;

use Tonic\Response;
use PDO;


/**
 * This class defines an example resource that is wired into the URI /example
 * @uri /transactions/:id
 */
class TransactionResource extends Transaction {
	/**
	 * @method GET
	 */
	function display() {
		$st = $this->db->prepare("SELECT * FROM `transaction` WHERE `id` = :id");
		$st->execute(array("id" => $this->id));
		if(($data = $st->fetch(PDO::FETCH_ASSOC)) !== false) {
			return new Response(Response::OK, $data);
		}
		else {
			return new Response(Response::NOTFOUND);
		}
	}

	/**
	 * @method PUT
	 */
	function update() {
		$st = $this->db->prepare("UPDATE `transaction` SET ".$this->pdoSet($values)." WHERE  `id` = :id");
		$values["id"] = $this->id;
		if($st->execute($values)) {
			if($st->rowCount() === 1) {
				$st = $this->db->prepare("UPDATE `user` SET `total` = `total` + :price WHERE  `id` = :user_id");
				if($st->execute($values)) {
					if($st->rowCount() === 1) {
						return new Response(Response::CREATED);
					}
					else {
						return new Response(Response::NOTFOUND);
					}
				}
				else {
					return new Response(Response::BADREQUEST);
				}
			}
			else {
				return new Response(Response::NOTFOUND);
			}
		}
		else {
			return new Response(Response::BADREQUEST);
		}
	}

	/**
	 * @todo: Implement by setting a deleted timestamp
	 * @method DELETE
	 */
	function remove() {
// 		$ds = $this->container['dataStore'];
// 		$ds->delete($this->id);
		return new Tonic\Response(Tonic\Response::NOCONTENT);
	}
}