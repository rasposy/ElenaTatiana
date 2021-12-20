document.getElementById("search").addEventListener("click", function(){
    var url = "http://ptsv2.com/t/e3nzb-1639946066/post";

    var xhr = new XMLHttpRequest();
    xhr.open("POST", url, false);
  //var min = document.getElementById("boundryMin");
  //var max = document.getElementById("boundryMax");
  //var obj = {boundryMin:min, boundryMax:max};
  //var objjson = JSON.stringify(obj);
  xhr.setRequestHeader("Content-Type", "application/json");
  xhr.send("test");
  console.log(xhr.response);
})