opt_mu = function(z, K, d){
    mu = matrix(NA, nrow = d, ncol = K)

    n_k = rep(NA, length = K)
    for(k in 1:K){
        n_k[k] = length(z[z==k])
    }
    for(j in 1:d){
        for(k in 1:K){
            mu[j, k] = (1/n_k[k])*sum(x[z==k, j])
        }
    }
    return(mu)
}

opt_z = function(x, K, mu){
    n = nrow(x)
    z = rep(NA, length = n)

    for(i in 1:n){
        distance = rep(NA, length = K)
        for(k in 1:K){
            distance[k] = t(x[i, ] - mu[, k])%*%(x[i, ] - mu[, k])
        }
        z[i] = (1:K)[distance==min(distance)][1]
    }

    return(z)
}

loss = function(x, z, mu){
    loss = 0

    n = nrow(x)
    for(i in 1:n){
        loss = loss + (1/n)*c(t(x[i, ] - mu[, z[i]])%*%(x[i, ] - mu[, z[i]]))
    }

    return(loss)
}

kmeans = function(x, K, iter_max = 1000, epsilon = 1e-10){
    n = nrow(x)
    d = ncol(x)
    l = rep(NA, iter_max+1)
    z = sample(x = 1:K, size = n, replace = TRUE)
    mu = opt_mu(z = z, K = K, d = d)
    l[1] = Inf
    for(i in 1:iter_max){
        z = opt_z(x = x, K = K, mu = mu)
        mu = opt_mu(z = z, K = K, d = d)
        l[i+1] = loss(x = x, z = z, mu = mu)
        converge = abs(l[i+1] - l[i]) < epsilon
        if(converge){
            iter_num = i
            message("converged")
            break
        }
        cat("iteration numer is", i, "\n")
    }

    args_list = list(
        x = x,
        z = z,
        l = l[2:iter_max],
        iter_num = iter_num
    )
    return(args_list)
}
