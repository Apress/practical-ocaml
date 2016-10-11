0x10;;
0x10 + 10;;

#load "unix.cma";;
#load "binary_match.cmo";;

Binary_match.find_matching_locations "/bin/ls" "/usr/bin/who";;
Binary_match.find_matching_locations ~min_match:3 "/bin/ls" "/usr/bin/who";;
Binary_match.longest_match "/bin/ls" "/usr/bin/who";;
Binary_match.show_distribution "/bin/ls" "/usr/bin/who";;

#load "bitmap.cmo";;

Bitmap.custom_emptybmp "random.bmp" 420 300 (fun x -> Char.chr (Random.int 255));;

Bitmap.xorimage "random.bmp" "sample.bmp" "random3.bmp";;
Bitmap.landimage "random.bmp" "sample.bmp" "random4.bmp";;

#load "graphics.cma";;
#load "life.cmo";;
module CGL = Life.Game(Life.Default);;
CGL.game_of_life "outputfile.bmp" 10 10;;
CGL.make_record 100 10 "output-string.bmp" 10;;

module GraGCL = Life.GraphicGame(Life.Default);;
GraGCL.init ();;
GraGCL.game_of_life 100 20;;
GraGCL.close ();;
