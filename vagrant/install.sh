sudo apt-get install libtool

cd ~
wget https://ftp.gnu.org/gnu/gmp/gmp-6.0.0a.tar.lz
lzcat gmp-6.0.0a.tar.lz | tar x
cd gmp-6.0.0a

sudo apt-get install libgmp3-dev

cd ~
wget ftp://ftp.gnu.org/gnu/guile/guile-2.0.11.tar.gz
zcat guile-2.0.11.tar.gz | tar xvf -
cd guile-2.0.11
./configure
make
make install
