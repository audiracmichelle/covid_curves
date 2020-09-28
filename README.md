# covid_curves
Characterization of covid curves 

```
cd $(pwd)
docker build -t interrupted-time-series .
docker run --rm -e PASSWORD=rstan -p 8787:8787 -v $(pwd):/home/rstudio/kitematic/ interrupted-time-series
```

https://github.com/Priesemann-Group/covid19_inference_forecast