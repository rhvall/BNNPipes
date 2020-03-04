convert -loop 0 -dispose 'Previous' output1.gif output2.gif output3.gif oo.gif
convert -loop 0 -dispose 'Previous' *.png output.gif
convert -loop 0 -dispose 'Previous' -resize 250x800 Gen50/*.png output0.gif

for i in *; do convert -resize 250x800 $i $i.png; done
for i in *; do convert -resize 250x600\! $i $i.png; rm $i; done;

NUM=0
for i in *; do mv $i $NUM.png; NUM=$((NUM+1)); done;


ffmpeg -i %d.png -r 1 -b:v 5000 -c:v png -vf "[in] split [T1],fifo, lutrgb=r=0:g=0:b=0, pad=in_w:in_h:0:0:0x000000, [T2] overlay [out]; [T1] fifo, pad=in_w:in_h:0:0:0x000000[T2]" -y out.mp4

ffmpeg -i %d.png -r 30 -vcodec libx264 -pix_fmt yuv420p out.mp4

for i in *; do convert -flatten $i $if.png; rm $i; done;

for i in *-*.png; do mv "$i" "${i/ - /}"; done

for i in ?.png; do echo $i;done

for i in *.png; do convert $i -background "#212121" -flatten $i-; rm $i; done;

for i in *; do mv $i "${i/-/}";done;

convert -loop 0 -dispose 'Previous' *.png output.gif

convert -loop 0 -dispose 'Previous' -delay 1  *.png output.gif

convert -loop 0 -dispose 'Previous' -delay 5 *.png output.gif

convert -loop 0 -dispose 'Previous' -delay 100 *.png output.gif

convert -loop 0 -dispose 'Previous' -delay 25 *.png output.gif

# Change multiple file names at the same time that keeps the number
# Example File name: Possible Minds Twenty-Five Ways of Looking at AI - 01.mp3
for i in "Possible Minds"*; do NUMBER=$(echo $i | awk -F'[- .]' '{print $12}');mv "$i" PossibleMinds-$NUMBER.mp3; done;