#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix Cgd(int maxIter, NumericMatrix inputData, NumericMatrix theta, int rowLength, NumericMatrix temporaryTheta, NumericVector outputData,float alpha)
{
    for (int iteration = 0; iteration < maxIter; iteration++)
    {
        NumericMatrix tran_theta = transpose(theta);
        //inputData %*% t(theta)
        NumericVector y_hat(inputData.nrow());
        NumericVector temp(inputData.ncol());
        for (int i = 0; i < inputData.nrow();i++)
        {
            temp = inputData(i,_) * tran_theta;
            y_hat[i] = sum(temp);
        }
        NumericVector error = y_hat - outputData;
        for (int j = 0; j < theta.ncol(); j++)
        {
            NumericVector term = error * inputData(_,j);
            //calculate gradient
            float gradient = sum(term) / rowLength;
            //printf("gradient: %f\n",gradient);
            temporaryTheta(0,j) = theta(0,j) - (alpha * gradient);
        }
        theta = temporaryTheta;
    }
    return theta;
}