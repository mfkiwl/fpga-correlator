## Tools for TART FPGA programming

Two scripts. Run both to install the necessary tools.


## Gowin Tools from the Command Line
Guides here
* (http://cdn.gowinsemi.com.cn/SUG113E.pdf) Design Guide
* (http://cdn.gowinsemi.com.cn/SUG550E.pdf)
* ()


```
Gowin logic synthesis tool.
Usage: GowinSynthesis [-h|--help] [-i verilog_file [file...]] [-if input_file] [-inc path[path...]] [-dir directory[directory...]] [-o output_file] [-top top_module] [-gsc gsc_file] [-sdc sdc_file] [-prj project_file] [-p dev_info] [-v|-version] [-verilog_language verilog version] [-vhdl_language vhdl version] [-do dofile] [-pn partNumber] [-disable_insert_pad 0/1] [-ram_rw_check 0/1] [-looplimit loopvalue] [-gao gaoFiles] [-i2c_bgp i2cBcpFileS] [-active_flash activeFlashFileS] [-oh output_vhdl_file] [-ot output_tmp_file] [-dsp_balance] [-ram_balance] [-lib library1 library2 etc] 

Arguments:
    -h|--help                            Show this help message and exit
    -i verilog_file [file...]            Add verilog files
    -if input_file                       Add verilog files by a file
    -inc path[path...]                   Add include paths
    -dir directory[directory...]         Add all verilog files in the directory
    -o output_file                       Specify the output file name
    -top top_module                      Specify the top module name
    -gsc gsc_file                        Set property constraint file
    -sdc sdc_file                        Set timing constraint file
    -prj project_file                    Specify the project file
    -p dev_info                          Specify the device information, include device part, package and speed info
    -v|-version                          Show the version of GowinSyn
    -verilog_language verilog version    Specify use verilog-1995 or verilog-2001 or sysv-2017 only
    -vhdl_language vhdl version          Specify use vhdl-1993 or vhdl-2008
    -do dofile                           Run gowinSynthesis by command options in dofile
    -pn partNumber                       set partNumber
    -disable_insert_pad 0/1              Disable I/O Insertion
    -ram_rw_check 0/1                    Automatic Read/Write Check Insertion for RAM
    -looplimit loopvalue                 Specify a loop iteration value for while loops for design
    -gao gaoFiles                        Set gao file
    -i2c_bgp i2cBcpFileS                 Add i2c bcp file
    -active_flash activeFlashFileS       Add active flash file
    -oh output_vhdl_file                 Specify the output vhdl file name
    -ot output_tmp_file                  Specify the output IPCore template file name
    -dsp_balance                         set dsp balance
    -ram_balance                         set ram balance
    -lib library1 library2 etc           set library with its file

```
