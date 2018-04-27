
if [ "$(uname -m)" = 'x86_64' ];
  then
  echo "Installing dependencies Be Patient"
   apt-get update
   apt-get install libghc-juicypixels-dev
   apt-get install libghc-repa-dev
   apt-get install libghc-gtk-dev
fi

