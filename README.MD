
---
EPM
## Contents

- [Contents](#contents)
- [About](#about)
- [Quick start](#quick-start)
- [Works deploying EPM](#works)
- [Documentation](#documentation)
- [Citing EPM](#citing-EPM)
- [License](#license)

---

## About

Electricity Planning Model (EPM) is a least-cost planning framework. It minimizes the costs of expanding and operating a power system while meeting the model's technical, economic, and environmental requirements. EPM is a long-term planning model, which means it optimizes the annual capacity additions based on system costs over multiple years, including fixed (annualized capital and fixed operation and maintenance [O&M]) costs and variable (fuel and variable O&M) costs. Moreover, EPM addresses the dispatch of the generators, decides on the activities per geographical zone and the exchange between them. Furthermore, the model suggests the allocation of spinning reserves among generators and allows for implementing different policies, e.g., emissions limits, fuel, and import limits, spinning reserve requirements, transmissions caps, ramp limits, or carbon prices.

EPM has been developed by the Power Systems Planning Group, embedded in the Energy Sector Management Assistance Program (ESMAP). As power system planning is one of the key activities performed by energy ministries and utilities around the world, the essential goal of this project is to actively support the operational work of the World Bank's staff and clients for appropriate decision-making within the energy industry. Furthermore, EPM is designed to be a financially attractive alternative to high-end commercial tools without compromising the available features and applications.

EPM is formulated in the General Algebraic Modeling System (GAMS). While GAMS runs EPM, all input data is provided via an Excel interface to the model. Likewise, the results are collected in an Excel output file. Hence, users only interact with GAMS upon installation and afterward exclusively use the Excel interface. Consequently, the GAMS programming language knowledge is not a prerequisite for the basic runs.

The practical deployment of EPM consists of a 7-step process, which is illustrated below.

<img src="https://i.postimg.cc/jdZH17w9/Screenshot-2022-01-14-102001.png" alt="drawing" width="300" align="center"/>

## Quick start
EPM is a model formulated in the General Algebraic Modeling System (GAMS), a high-level modeling system for mathematical optimization. GAMS and Excel are required to be installed to run EPM. Furthermore, to work with EPM, additional licensing of GAMS is necessary. To install GAMS or obtain the appropriate license, go to the following website and follow the provided instructions:

https://www.gams.com/download/

The Excel file, labeled *EPM_input.xlsx*, serves the interface of the EPM tool. This file should store all the input data required to run the model. When using the EPM interface, it is advisable to address one tab at a time and start at the left-most tab and sequentially address the right-neighboring tab. When the Excel file is appropriately filled, the user might proceed to the GAMS software and run the model. Otherwise, the run will produce errors, which need to be addressed.

Model is composed of three .gms files:
- _*base.gms_   - contains the introduction of all the variables, parameters, and equations of the model.
- _*report.gms_ - processes the model's outcomes and produces the results files.
- _*main.gms_   - controls the overall flow of the program and is used to run the model.

To run the model, open _*main.gms_ file in GAMS IDE or GAMS Studio in the same directory with _*report.gms_, _*base.gms_ and filled *EPM_input.xlsx*, and run the script. The model will automatically include all the input data and files, solve the model and produce the results.

## Works deploying EPM

- Bankuti, Miklos; Chattopadhyay, Debabrata; Song, Chong-Suk. 2018. Integrating Variable Renewable Energy in the Bangladesh Power System : A Planning Analysis. Policy Research Working Paper;No. 8517. World Bank, Washington, DC. © World Bank. https://openknowledge.worldbank.org/handle/10986/29990
- World Bank. 2021. The Value of Trade and Regional Investments in The Pan-Arab Electricity Market : Integrating Power Systems and Building Economies. World Bank, Washington, DC. © World Bank. https://openknowledge.worldbank.org/handle/10986/36614
- Timilsina, Govinda; Deluque Curiel, Ilka; Chattopadhyay, Deb. 2021. How Much Does Latin America Gain from Enhanced Cross-Border Electricity Trade in the Short Run?. Policy Research Working Paper;No. 9692. World Bank, Washington, DC. © World Bank. https://openknowledge.worldbank.org/handle/10986/35729
- World Bank Group. Poland Energy Transition: The Path to Sustainability in the Electricity and Heating Sector. World Bank, 2018.
- Chattopadhyay, D., Chitkara, P., Curiel, I. D., & Draugelis, G. (2020). Cross-Border Interconnectors in South Asia: Market-Oriented Dispatch and Planning. IEEE Access, 8, 120361-120374.
- Huang, Z., Smolenova, I., Chattopadhyay, D., Govindarajalu, C., De Wit, J., Remy, T., & Curiel, I. D. (2021). ACT on RE+ FLEX: Accelerating Coal Transition Through Repurposing Coal Plants Into Renewable and Flexibility Centers. IEEE Access, 9, 84811-84827.
- Remy, T., & Chattopadhyay, D. (2020). Promoting better economics, renewables and CO2 reduction through trade: A case study for the Eastern Africa Power Pool. Energy for Sustainable Development, 57, 81-97.
- Islam, M. E., Khan, M. M. Z., Chattopadhyay, D., & Väyrynen, J. (2021). Impact of COVID-19 on dispatch and capacity plan: A case study for Bangladesh. The Electricity Journal, 34(5), 106955.
- Islam, M. E., Khan, M. M. Z., Chattopadhyay, D., & Draugelis, G. (2020, August). Economic benefits of cross border power trading: A case study for Bangladesh. In 2020 IEEE Power & Energy Society General Meeting (PESGM) (pp. 1-5). IEEE.
- Spyrou, E., Hobbs, B. F., Bazilian, M. D., & Chattopadhyay, D. (2019). Planning power systems in fragile and conflict-affected states. Nature energy, 4(4), 300-310.
- Suski, A., Remy, T., Chattopadhyay, D., Song, C. S., Jaques, I., Keskes, T., & Li, Y. (2021). Analyzing Electric Vehicle Load Impact on Power Systems: Modeling Analysis and a Case Study for Maldives. IEEE Access, 9, 125640-125657.

## Documentation

Documentation is available under the following links:

 - here the links to the documents in doc folder (Manual and Mathematical Formulations): update the manual

## Citing EPM

Please cite EPM as:

To update with latest formulation document. Chattopadhyay, D., De Sisternes, F., Oguah, S. K. W., World Bank Electricity Planning Model (EPM) Mathematical Formulation, 2018, Energy Sector Management Assistance Program (ESMAP), International Bank for Reconstruction and Development, The World Bank, Washington DC

## License

Licensed under the Areative Commons Zero v1.0 Universal (the "License"); you
may not use this file except in compliance with the License. You may
obtain a copy of the License at

<https://creativecommons.org/publicdomain/zero/1.0/>
