if [ "$(uname -m)" = 'x86_64' ];
  then
  echo "Installing dependencies"
  echo "Wait for It"
  apt-get update
  apt-get install git
fi

