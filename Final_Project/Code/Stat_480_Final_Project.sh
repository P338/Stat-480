# Set working directory in terminal to grab airline files
cd ~/Stat480/RDataScience/AirlineDelays

# Unix command script to write 2003 and 2005 csv airlines data to a single file called Airlines0305.csv
cp 2003.csv 0305.csv
tail -n+2 2005.csv >> 0305.csv
