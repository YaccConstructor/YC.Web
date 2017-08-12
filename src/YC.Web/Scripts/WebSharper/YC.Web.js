(function()
{
 var Global=this,Runtime=this.IntelliFactory.Runtime,List,Html,Client,Attr,Tags,T,Concurrency,EventsPervasives,Operators,Remoting,AjaxRemotingProvider;
 Runtime.Define(Global,{
  YC:{
   Web:{
    Client:{
     HelloWorld:function()
     {
      var x,Welcome,x1,Output,arg10,arg101,x2,arg00;
      x=List.ofArray([Attr.Attr().NewAttr("value","")]);
      Welcome=Tags.Tags().NewTag("input",x);
      x1=Runtime.New(T,{
       $:0
      });
      Output=Tags.Tags().NewTag("h2",x1);
      arg101=List.ofArray([Tags.Tags().text("Click me")]);
      x2=Tags.Tags().NewTag("button",arg101);
      arg00=function()
      {
       return function()
       {
        return Concurrency.Start(Concurrency.Delay(function()
        {
         Output.set_Text("Hello, world!");
         return Concurrency.Return(null);
        }),{
         $:0
        });
       };
      };
      EventsPervasives.Events().OnClick(arg00,x2);
      arg10=List.ofArray([Welcome,x2]);
      return Tags.Tags().NewTag("div",arg10);
     },
     Main:function()
     {
      var input,arg10,x,output,arg101,x1,x2,arg00,arg102,arg103,arg104;
      arg10=List.ofArray([Attr.Attr().NewAttr("value","")]);
      input=Operators.add(Tags.Tags().NewTag("input",arg10),Runtime.New(T,{
       $:0
      }));
      x=Runtime.New(T,{
       $:0
      });
      output=Tags.Tags().NewTag("h1",x);
      x1=List.ofArray([Tags.Tags().text("Send")]);
      x2=Tags.Tags().NewTag("button",x1);
      arg00=function()
      {
       return function()
       {
        var arg001;
        arg001=Concurrency.Delay(function()
        {
         return Concurrency.Bind(AjaxRemotingProvider.Async("YC.Web:0",[input.get_Value()]),function(_arg11)
         {
          output.set_Text(_arg11);
          return Concurrency.Return(null);
         });
        });
        return Concurrency.Start(arg001,{
         $:0
        });
       };
      };
      EventsPervasives.Events().OnClick(arg00,x2);
      arg102=Runtime.New(T,{
       $:0
      });
      arg103=List.ofArray([Attr.Attr().NewAttr("class","text-muted")]);
      arg104=List.ofArray([Attr.Attr().NewAttr("class","jumbotron")]);
      arg101=List.ofArray([input,x2,Tags.Tags().NewTag("hr",arg102),Operators.add(Tags.Tags().NewTag("h4",arg103),List.ofArray([Tags.Tags().text("The server responded:")])),Operators.add(Tags.Tags().NewTag("div",arg104),List.ofArray([output]))]);
      return Tags.Tags().NewTag("div",arg101);
     },
     Start:function(input,k)
     {
      var arg00;
      arg00=Concurrency.Delay(function()
      {
       return Concurrency.Bind(AjaxRemotingProvider.Async("YC.Web:0",[input]),function(_arg1)
       {
        return Concurrency.Return(k(_arg1));
       });
      });
      return Concurrency.Start(arg00,{
       $:0
      });
     }
    }
   }
  }
 });
 Runtime.OnInit(function()
 {
  List=Runtime.Safe(Global.WebSharper.List);
  Html=Runtime.Safe(Global.WebSharper.Html);
  Client=Runtime.Safe(Html.Client);
  Attr=Runtime.Safe(Client.Attr);
  Tags=Runtime.Safe(Client.Tags);
  T=Runtime.Safe(List.T);
  Concurrency=Runtime.Safe(Global.WebSharper.Concurrency);
  EventsPervasives=Runtime.Safe(Client.EventsPervasives);
  Operators=Runtime.Safe(Client.Operators);
  Remoting=Runtime.Safe(Global.WebSharper.Remoting);
  return AjaxRemotingProvider=Runtime.Safe(Remoting.AjaxRemotingProvider);
 });
 Runtime.OnLoad(function()
 {
  return;
 });
}());
