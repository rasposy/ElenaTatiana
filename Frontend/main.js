electron = require('electron');

app = electron.app; 
menu = electron.Menu;
BrowserWindow = electron.BrowserWindow;

var mainWindow = null;

var myMenu = menu.buildFromTemplate([]);

menu.setApplicationMenu(myMenu);

app.on('window-all-closed', function(){
    if(process.platform !== 'darwin') app.quit();
    app.quit();
    app.exit(0);
});

app.on('ready', function(){
    mainWindow = new BrowserWindow({
        width: 750,
        height: 1000,
        icon: __dirname + '/icons/Pprimy.png'});
    url = 'file://' + __dirname + '/frontend.html';
    mainWindow.loadURL(url);
    mainWindow.on('closed', function(){
        mainWindow = app.exit(0);
    });
});