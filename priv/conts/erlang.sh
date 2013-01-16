set -e

sudo apt-get update
sudo apt-get install -y git build-essential erlang
sudo apt-get build-dep -y erlang

cd ~
curl -O https://raw.github.com/spawngrid/kerl/master/kerl
chmod a+x kerl
sudo cp kerl /usr/local/bin/
kerl update releases

for v in R14B02 R14B03 R14B04 R15B R15B01 R15B02 R15B03; do
    cd
    echo
    echo "Fetching and building $v"
    b=`echo $v | awk '{print tolower($0)}'`
    mkdir -p /opt/erlang/$b
    kerl build $v $b
    d="/opt/erlang/$b"

    echo
    echo "Installing $v in $d"
    kerl install $b $d

    echo
    echo "Building rebar for $v"
    . $d/activate
    cd `mktemp -d`
    git clone git://github.com/rebar/rebar.git .
    ./bootstrap
    sudo cp rebar $(dirname $(which erl))
    kerl_deactivate
done
