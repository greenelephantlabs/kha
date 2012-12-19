sudo apt-get update
sudo apt-get install -y git build-essential erlang
cd `mktemp -d`
git clone git://github.com/rebar/rebar.git .
./bootstrap
sudo cp rebar /usr/local/bin/
