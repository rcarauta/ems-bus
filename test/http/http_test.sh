#!/bin/bash 
#
# http test search
# Author: Everton de Vargas Agilar
# Data: 16/03/2016
#

if [ $1 == "" ]; then
	echo "Uso: http_test.sh count_reqs"
else
	 COUNTER=$1
	 echo "$COUNTER asynchronous http requests to the ErlangMS..."
	 until [  $COUNTER -lt 0 ]; do
		 let COUNTER-=1
		 curl -vX GET localhost:2301/info &
	 done
	 echo "done!"
fi


			         
