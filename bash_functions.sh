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

function catWithOmission()
{
    #Assumes you want to do it in the local directory
    FILES=./*
    NO_TOUCH="$1"
    COMM=""
    for f in $FILES
    do
    if [[  $f =~ "$NO_TOUCH" ]]; then
    continue
    fi
    if [[ -f "$f" ]]; then
    echo "Processing $f file...";
    COMM="$COMM $f"
    
    fi
    done
    cat $COMM > $2
}
