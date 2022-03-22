<!-- 
Add a project state badge

See <https://github.com/BCDevExchange/Our-Project-Docs/blob/master/discussion/projectstates.md> 
If you have bcgovr installed and you use RStudio, click the 'Insert BCDevex Badge' Addin.
-->

[![img](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

R.ccdc.tools
============

Description of package

Simple R tools for working with Continuous Change Detection and Classification (CCDC) (Zhu & Woodcock 2014) images.

### Features
Includes various tools for creating derivative products from CCDC images output from Google Earth Engine including: plotting surface reflectance time series, getting CCDC coefficients for specific dates, creating 'synthetic' images and a few extras. 

### Installation
```{r}
remotes::install_github("R.ccdc.tools")
```

### Usage
See [vignette](https://htmlpreview.github.io/?https://github.com/HunterGleason/R.ccdc.tools/blob/main/vignettes/ccdc-vignette.html) for specific usage examples. 

### Project Status
Currently maturing, developed for Landsat time series, working to incorporating Sentinel-2. 

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/R.ccdc.tools/issues/).

### How to Contribute

If you would like to contribute to the package, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2022 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```

---
*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.* 
