showHeader(){
        head -n 1 $1 | tr "\t" "\n" | nl
}
showHeaderSpace()
{
        head -n 1 $1 | tr " " "\n" | nl
}

export PATH=$PATH:/home-1/aomdahl1@jhu.edu/.bin/bedtools2/bin
function bam2sam()
{
        ml samtools
        samtools view -h $1 > $2
}

function transpose()
{
        awk '{for (f=1;f<=NF;f++) col[f] = col[f]":"$f} END {for (f=1;f<=NF;f++) print col[f]}' $1 | tr ':' '\t'

}
