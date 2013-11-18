HW=6
TASK=$1

if [ $TASK = 1 -o $TASK = 8 ];
then
  HW=$HW TASK=$TASK ./rkt-grade.sh
else 
  HW=$HW TASK=$TASK ./ml-grade.sh
fi

