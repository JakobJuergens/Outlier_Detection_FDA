#include <Rcpp.h>
#include <math.h>
#include <vector>
#include <cassert>
#include <numeric>
#include <algorithm>
#include <functional>
using namespace Rcpp;

double interpolate(double x_left, double x_right, double y_left, double y_right, double x){
  if(x == x_left) return(y_left);
  double m = (y_right - y_left) / (x_right - x_left);
  double y = y_left + m * (x - x_left);
  return(y);
}

double x_lneigh(NumericVector args, double x) {
  int len = args.size();
  int i = 0;
  
  while(i < len){
    if(x >= args[i]){
      if(x == args[i]) return(i);
      i++;
    }
    else{
      break;
    }
  } 
  return(i-1);
}

// [[Rcpp::export]]
NumericVector grid_approx_obs(NumericVector args, NumericVector vals, NumericVector grid) {
  
  int n_points = grid.size();
  NumericVector output(n_points);
  int x_l = 0;
  
  for(int i = 0; i < n_points; i++){
    x_l = x_lneigh(args, grid[i]);
    output(i) = interpolate(args[x_l], args[x_l + 1], vals[x_l], vals[x_l + 1], grid[i]);
  }
  
  return(output);
}