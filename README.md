### What is OPEX?

OPEX is an optimal experimental design framework to help biologists to select the most informative experiments to conduct given the experiments conducted up to now. This repo demonstrates the application of OPEX on collecting gene expression data of <em>E. coli<em> under the stress of various antibiotics and biocides.

### Dependencies
* [mlegp](https://cran.r-project.org/web/packages/mlegp/index.html)

### Structure of this project

### Input data
The input data is a table, in which the first 14 columns define the culture conditions in each row and the other 1123 columns represents the gene expression profile for each condition. (Genes that did not have a sufficient sequecing depth were excluded). 

A culutre condition is defined by a binary vecotr, representing the presence (with 0) or absence (with 1) of 10 biocides and 4 antibiotics: ```Chlorexidine, Phenol, H2O2, Isopropanol, Bezalkonium_chloride, Ethanol, Glutaraldehyde, Percetic_acid, Sodium_hypochlorite, Povidone_iodine, Kanamycin, Rifampicin, Norfloxacin, Ampicillin```

### How to run
* Step1: generate a file that include the configurations for running OPEX.
  ```
  cd generate_setting
  Rscript generate_setting.R
  ```
  After running the above commands, a file named ```generate_setting.csv``` is generated in ```./out_data```. The ```generate_setting.csv``` specifies the value for hyper-parameters: random_seed, exploration frequency, adaptive , start size, add, dataset id, noise, iter_num, and method.

* Step2: Run the simulation on one configuration.
```
cd code
bash run_one_setting.sh
```

Upon completion, a folder named ```expert_sample``` will be created in ```./out_data```. The folder has the same name as the Rscript used for generating configurations in Step 1. Inside the folder, ```expert_sample```, there is a folder for each sampling approach.

The result is a csv file named by the configuration and contains the order of each culture condition selected by expert sampling.

### Code architecture
In this dir, ```./code/workhorse```, there are seven R scripts as follows. In ```main.R```, an object of the ```Simulate``` class is defined according to a configuration determined by a command line argument. The defintion of the ```Simulate``` class is defined in ```OOPGP.R```. The major method in the ```Simulate``` class is the ```simulate``` method. All the other methods and functions in other files are for supporting the ```simulate``` method of the ```Simulate``` class.

```
├── generate_setting
│   └── expert_sampling.R
├── run_one_setting.sh
├── setpath.R
└── workhorse
    ├── add_noise.R
    ├── main.R
    ├── max_dist.R
    ├── OOPGP.R
    ├── prepare_data.R
    ├── screen_index_helper.R
    └── update_train_pool.R

```


### Acknowledgement
This work was supported by an NSF award (#1743101).


