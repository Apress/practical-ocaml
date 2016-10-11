let genre_map = [
  (0,"Blues");  (1,"Classic Rock");  (2,"Country");  (3,"Dance");
  (4,"Disco");  (5,"Funk");  (6,"Grunge");  (7,"Hip-Hop");
  (8,"Jazz");  (9,"Metal");  (10,"New Age");  (11,"Oldies");
  (12,"Other");  (13,"Pop");  (14,"R&B");  (15,"Rap");  (16,"Reggae");
  (17,"Rock");  (18,"Techno");  (19,"Industrial");  (20,"Alternative");
  (21,"Ska");  (22,"Death Metal");  (23,"Pranks");  (24,"Soundtrack");
  (25,"Euro-Techno");  (26,"Ambient");  (27,"Trip-Hop");  (28,"Vocal");
  (29,"Jazz+Funk");  (30,"Fusion");  (31,"Trance");  (32,"Classical");
  (33,"Instrumental");  (34,"Acid");  (35,"House");  (36,"Game");
  (37,"Sound Clip");  (38,"Gospel");  (39,"Noise");  (40,"Alternative Rock");
  (41,"Bass");  (43,"Punk");  (44,"Space");  (45,"Meditative");
  (46,"Instrumental Pop");  (47,"Instrumental Rock");  (48,"Ethnic");
  (49,"Gothic");  (50,"Darkwave");  (51,"Techno-Industrial");
  (52,"Electronic");  (53,"Pop-Folk");  (54,"Eurodance");  (55,"Dream");
  (56,"Southern Rock");  (57,"Comedy");  (58,"Cult");  (59,"Gangsta");
  (60,"Top 40");  (61,"Christian Rap");  (62,"Pop/Funk");  (63,"Jungle");
  (64,"Native US");  (65,"Cabaret");  (66,"New Wave");  (67,"Psychadelic");
  (68,"Rave");  (69,"Showtunes");  (70,"Trailer");  (71,"Lo-Fi");
  (72,"Tribal");  (73,"Acid Punk");  (74,"Acid Jazz");  (75,"Polka");
  (76,"Retro");  (77,"Musical");  (78,"Rock & Roll");  (79,"Hard Rock");
  (80,"Folk");  (81,"Folk-Rock");  (82,"National Folk");  (83,"Swing");
  (84,"Fast Fusion");  (85,"Bebob");  (86,"Latin");  (87,"Revival");
  (88,"Celtic");  (89,"Bluegrass");  (90,"Avantgarde");  (91,"Gothic Rock");
  (92,"Progressive Rock");  (93,"Psychedelic Rock");  (94,"Symphonic Rock");
  (95,"Slow Rock");  (96,"Big Band");  (97,"Chorus");  (98,"Easy Listening");
  (99,"Acoustic");  (100,"Humour");  (101,"Speech");  (102,"Chanson");
  (103,"Opera");  (104,"Chamber Music");  (105,"Sonata");
  (106,"Symphony");  (107,"Booty Bass");  (108,"Primus");  (109,"Porn Groove");
  (110,"Satire");  (111,"Slow Jam");  (112,"Club");  (113,"Tango");
  (114,"Samba");  (115,"Folklore");  (116,"Ballad");  (117,"Power Ballad");
  (118,"Rhytmic Soul");  (119,"Freestyle");  (120,"Duet");  (121,"Punk Rock");
  (122,"Drum Solo");  (123,"Acapella");  (124,"Euro-House");
  (125,"Dance Hall");  (126,"Goa");  (127,"Drum & Bass");  (128,"Club-House");
  (129,"Hardcore");  (130,"Terror");  (131,"Indie");  (132,"BritPop");
  (133,"Negerpunk");  (134,"Polsk Punk");  (135,"Beat");
  (136,"Christian Gangsta");  (137,"Heavy Metal");  (138,"Black Metal");
  (139,"Crossover");  (140,"Contemporary C");  (141,"Christian Rock");
  (142,"Merengue");  (143,"Salsa");  (144,"Thrash Metal");
  (145,"Anime");  (146,"JPop");  (147,"SynthPop")];;

let string_of_genre g = try 
  List.assoc g genre_map
with Not_found -> "Unknown";;

type id3tag = {song_title:string;
	       artist:string;
	       album: string;
	       year: int;
	       comment: string;
	       track: int;
	       genre: char };;

let empty_tag = {song_title = "";
	       artist="";
	       album="";
	       year=0;
	       comment="";
	       track=0;
	       genre='\000'};;

exception BadTag of string;;

let rstrip str = let lastchar = ref str.[0] in 
let buf = Buffer.create (String.length str) in
  String.iter (fun x -> match lastchar.contents,x with
		   ' ',' ' -> lastchar := x
		 | _,'\000' -> ()
		 | ' ',_ -> Buffer.add_char buf lastchar.contents;lastchar := x
		 | _,' ' -> Buffer.add_char buf lastchar.contents;lastchar := x
		 | _,_ -> Buffer.add_char buf lastchar.contents;lastchar := x
) (String.sub str 1 ((String.length str) - 1));
  Buffer.contents buf;;

let parse_id3tag x = let tagdata = String.sub x 0 3 in
  match tagdata with
      "TAG" -> { song_title = rstrip (String.sub x 3 29);
		 artist = rstrip (String.sub x 33 29);
		 album = rstrip (String.sub x 63 29);
		 year = Scanf.sscanf (String.sub x 93 4) "%i" (fun x -> x);
		 comment = rstrip (String.sub x 97 27);
		 track = int_of_char x.[126];
		 genre = x.[127] }
    | _ -> raise (BadTag x);;
		 
let findtag fname = let ic = open_in_bin fname in
let sz = in_channel_length ic in
  let s = String.create 128 in
    seek_in ic (sz - 128);
    really_input ic s 0 128;
    s;;

let getid3 file = parse_id3tag (findtag file);;
  
let zero_string len = let q = String.create len in
let rec zs str index = if (index < len) then
  (str.[index] <- '\000';zs str (index + 1))
else
  str
in
  zs q 0;;

let pad str len = let z = zero_string len in 
  if ((String.length str) < len) then
    (String.sub str 0 (String.length str)) ^ (String.sub z (String.length str) (len - (String.length str)))
  else
    (String.sub str 0 len);;

let set_song_title id3t str = {id3t with song_title = str};;
let set_artist id3t str = {id3t with artist = str};;
let set_album id3t str = {id3t with album = str};;
let set_year id3t yr = {id3t with year = yr };;
let set_comment id3t str = {id3t with comment = str};;
let set_track id3t tr = {id3t with track = tr };;
let set_genre id3t g = { id3t with genre = g};;
let id3tag_to_string id3t = Printf.sprintf "TAG%s%s%s%s%s%c%c" 
  (pad id3t.song_title 30)
  (pad id3t.artist 30) (pad id3t.album 30)
  (pad (string_of_int id3t.year) 4) 
  (pad id3t.comment 29) 
  (Char.chr id3t.track) id3t.genre;;

