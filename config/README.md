# Configuration files

These files form the TART configuration. TART-3 config is a work in progress at the moment. The current config is 

* telescope_config.json: Generic information like name, position, number of antennas.
* calibrated_antenna_positions: X-Y-Z coordinates (in a local frame) of the antennas.
* tart_antenna_pairs.json: Contains the mapping between Correlator output array index, and i,j pairs for visibilities. Replaces the old permute.txt file.

The correlator permute generator should expect to use telescope_config.json as it's input file (for number of antennas) and output tart_antenna_pairs as the output file.

