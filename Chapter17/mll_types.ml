type port = Port of int | Siteport of int * int * string

type date_host = { date:string ; host:string }

type client = { c_id: int; c_port: port }

type other_side = Client of client | Peer of string

type log_entry = Connected of (date_host * other_side * string)
		 | Heartbeat of (date_host * other_side)
		 | Command of (date_host * client * string)
		 | Disconnect of (date_host * other_side * string)
