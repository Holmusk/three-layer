module.exports = {
    inline: true,
    historyApiFallback: true,
    proxy:{
        '/':{
            target: 'http://localhost:8080',
            secure: false
        }
    }
}
