if [ -z $1 ]
then
  fswatch -r0 -e ".*" -i "es6.js" src | xargs -0n 1 -I {} $0 {}
else
  NEXT=`echo $1 | sed -e "s/es6\.//"`
  babel $1 -o $NEXT
fi
