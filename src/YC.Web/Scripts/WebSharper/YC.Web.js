(function()
{
 var Global=this,Runtime=this.IntelliFactory.Runtime,Formlets,Formlet,YC,Web,WebComponents,AlgorithmsComponents,Remoting,AjaxRemotingProvider,List,Enhance,FormContainerConfiguration,Controls,Data,Strings,BioGraphClient,Html,Client,Tags,Array,Arrays,Attr,GraphParsingClient,FormButtonConfiguration,Operators,window,EventsPervasives,T,Control,FSharpEvent,FileReader,IntelliFactory,Formlets1,Base,Result,jQuery;
 Runtime.Define(Global,{
  YC:{
   Web:{
    BioGraphClient:{
     MainForm:Runtime.Field(function()
     {
      var _builder_,formlet,x,inputRecord,CssClass,fc,InputGrammarForm,_builder_1,formlet1,x1,inputRecord1,CssClass1,fc1,InputGraphForm,formlet5,x2,inputRecord2,CssClass2,fc2,InputForm,OutputForm,_builder_3,formlet6;
      _builder_=Formlet.Do();
      formlet=_builder_.Delay(function()
      {
       var mapping,list;
       mapping=function(grmName)
       {
        return[grmName,AjaxRemotingProvider.Sync("YC.Web:5",[{
         $:1
        },grmName])];
       };
       list=AjaxRemotingProvider.Sync("YC.Web:4",[{
        $:1
       }]);
       return _builder_.Bind(AlgorithmsComponents.InputAreaControl("Grammar",List.map(mapping,list)),function(_arg1)
       {
        return _builder_.Bind(AlgorithmsComponents.RangeControl("String Range","Min","Max"),function(_arg2)
        {
         return _builder_.Return([_arg1,_arg2]);
        });
       });
      });
      x=Formlet.Vertical(formlet);
      inputRecord=FormContainerConfiguration.get_Default();
      CssClass={
       $:1,
       $0:"tomiddle"
      };
      fc=Runtime.New(FormContainerConfiguration,{
       Header:inputRecord.Header,
       Padding:inputRecord.Padding,
       Description:inputRecord.Description,
       BackgroundColor:inputRecord.BackgroundColor,
       BorderColor:inputRecord.BorderColor,
       CssClass:CssClass,
       Style:inputRecord.Style
      });
      InputGrammarForm=Enhance.WithCustomFormContainer(fc,x);
      _builder_1=Formlet.Do();
      formlet1=_builder_1.Delay(function()
      {
       var mapping,list;
       mapping=function(grmName)
       {
        return[grmName,AjaxRemotingProvider.Sync("YC.Web:5",[{
         $:0
        },grmName])];
       };
       list=AjaxRemotingProvider.Sync("YC.Web:4",[{
        $:0
       }]);
       return _builder_1.Bind(AlgorithmsComponents.InputAreaControl("Graph",List.map(mapping,list)),function(_arg3)
       {
        var formlet2,formlet3,formlet4;
        formlet2=Controls.Checkbox(false);
        formlet3=Enhance.WithTextLabel("Draw Graph",formlet2);
        formlet4=Enhance.WithLabelLeft(formlet3);
        return _builder_1.Bind(Enhance.WithFormContainer(formlet4),function(_arg4)
        {
         return _builder_1.Return([_arg3,_arg4]);
        });
       });
      });
      x1=Formlet.Vertical(formlet1);
      inputRecord1=FormContainerConfiguration.get_Default();
      CssClass1={
       $:1,
       $0:"tomiddle"
      };
      fc1=Runtime.New(FormContainerConfiguration,{
       Header:inputRecord1.Header,
       Padding:inputRecord1.Padding,
       Description:inputRecord1.Description,
       BackgroundColor:inputRecord1.BackgroundColor,
       BorderColor:inputRecord1.BorderColor,
       CssClass:CssClass1,
       Style:inputRecord1.Style
      });
      InputGraphForm=Enhance.WithCustomFormContainer(fc1,x1);
      formlet5=Data.$(Data.$(Formlet.Return(function(grmInput)
      {
       return function(grphInput)
       {
        return[grmInput,grphInput];
       };
      }),InputGrammarForm),InputGraphForm);
      x2=Formlet.Horizontal(formlet5);
      inputRecord2=FormContainerConfiguration.get_Default();
      CssClass2={
       $:1,
       $0:"totop"
      };
      fc2=Runtime.New(FormContainerConfiguration,{
       Header:inputRecord2.Header,
       Padding:inputRecord2.Padding,
       Description:inputRecord2.Description,
       BackgroundColor:inputRecord2.BackgroundColor,
       BorderColor:inputRecord2.BorderColor,
       CssClass:CssClass2,
       Style:inputRecord2.Style
      });
      InputForm=Enhance.WithCustomFormContainer(fc2,x2);
      OutputForm=function(tupledArg)
      {
       var _arg5,_arg6,rng,grm,graph,drawGr,_builder_2,formlet2,x3,inputRecord3,CssClass3,fc3;
       _arg5=tupledArg[0];
       _arg6=tupledArg[1];
       rng=_arg5[1];
       grm=_arg5[0];
       graph=_arg6[0];
       drawGr=_arg6[1];
       _builder_2=Formlet.Do();
       formlet2=_builder_2.Delay(function()
       {
        var _arg20_,_arg21_,matchValue,patternInput,_,seqs,grOption,_1,graphOption,txt,seqs1,grOption1;
        _arg20_=rng[0];
        _arg21_=rng[1];
        matchValue=AjaxRemotingProvider.Sync("YC.Web:6",[grm,graph,_arg20_,_arg21_,drawGr]);
        if(matchValue.$==1)
         {
          seqs=matchValue.$1;
          grOption=matchValue.$0;
          if(grOption.$==1)
           {
            graphOption=grOption.$0;
            _1=[{
             $:1,
             $0:graphOption
            },Strings.Join("\n",seqs)];
           }
          else
           {
            _1=[{
             $:0
            },Strings.Join("\n",seqs)];
           }
          _=_1;
         }
        else
         {
          txt=matchValue.$0;
          _=[{
           $:0
          },txt];
         }
        patternInput=_;
        seqs1=patternInput[1];
        grOption1=patternInput[0];
        return _builder_2.Bind(AlgorithmsComponents.OutputAreaControl(seqs1,"Output"),function(_arg7)
        {
         return _builder_2.Bind(BioGraphClient.ShowImageControl(grOption1,drawGr,"canvas"),function(_arg8)
         {
          return _builder_2.Return([_arg7,_arg8]);
         });
        });
       });
       x3=Formlet.Horizontal(formlet2);
       inputRecord3=FormContainerConfiguration.get_Default();
       CssClass3={
        $:1,
        $0:"totop"
       };
       fc3=Runtime.New(FormContainerConfiguration,{
        Header:inputRecord3.Header,
        Padding:inputRecord3.Padding,
        Description:inputRecord3.Description,
        BackgroundColor:inputRecord3.BackgroundColor,
        BorderColor:inputRecord3.BorderColor,
        CssClass:CssClass3,
        Style:inputRecord3.Style
       });
       return Enhance.WithCustomFormContainer(fc3,x3);
      };
      _builder_3=Formlet.Do();
      formlet6=_builder_3.Delay(function()
      {
       return _builder_3.Bind(InputForm,function(_arg9)
       {
        return _builder_3.Bind(OutputForm(_arg9),function(_arg10)
        {
         return _builder_3.Return([_arg9,_arg10]);
        });
       });
      });
      return Formlet.Vertical(formlet6);
     }),
     MainFormRun:function()
     {
      var arg10;
      arg10=List.ofArray([BioGraphClient.MainForm().Run(function()
      {
       return null;
      })]);
      return Tags.Tags().NewTag("div",arg10);
     },
     ShowImageControl:function(grOption,drawGr,id)
     {
      var matchValue,src,_,_1,graphOption,arr,indx,f1,matchValue1,d,c,b,a,formlet;
      matchValue=[grOption,drawGr];
      if(matchValue[0].$==1)
       {
        if(matchValue[1])
         {
          graphOption=matchValue[0].$0;
          arr=Array(graphOption.edges.length);
          for(indx=0;indx<=graphOption.edges.length-1;indx++){
           f1=function(nuc)
           {
            return nuc.$==3?"U":nuc.$==1?"C":nuc.$==2?"G":"A";
           };
           matchValue1=Arrays.get(graphOption.edges,indx);
           d=matchValue1[3];
           c=matchValue1[2];
           b=matchValue1[1];
           a=matchValue1[0];
           Arrays.set(arr,indx,[a,b,f1(c),d]);
          }
          _1=Formlet.OfElement(function()
          {
           var arg10;
           arg10=List.ofArray([Attr.Attr().NewAttr("id",id)]);
           return AlgorithmsComponents.Graph("Graph Visualization",arr,graphOption.countOfVertex,Tags.Tags().NewTag("div",arg10));
          });
         }
        else
         {
          matchValue[0].$0;
          _1=Formlet.OfElement(function()
          {
           var arg10;
           arg10=List.ofArray([Attr.Attr().NewAttr("hidden","true")]);
           return Tags.Tags().NewTag("img",arg10);
          });
         }
        _=_1;
       }
      else
       {
        _=matchValue[1]?Formlet.OfElement(function()
        {
         var arg10;
         arg10=List.ofArray([Attr.Attr().NewAttr("hidden","true")]);
         return Tags.Tags().NewTag("img",arg10);
        }):Formlet.OfElement(function()
        {
         var arg10;
         arg10=List.ofArray([Attr.Attr().NewAttr("hidden","true")]);
         return Tags.Tags().NewTag("img",arg10);
        });
       }
      src=_;
      formlet=Enhance.WithLabelAbove(src);
      return Enhance.WithFormContainer(formlet);
     }
    },
    GraphParsingClient:{
     MainForm:Runtime.Field(function()
     {
      var _builder_,formlet,x,inputRecord,CssClass,fc,GrammarInputForm,_builder_1,formlet1,x1,inputRecord1,CssClass1,fc1,GraphInputForm,formlet6,x2,inputRecord2,CssClass2,fc2,InputForm,OutputForm,_builder_7,formlet9;
      _builder_=Formlet.Do();
      formlet=_builder_.Delay(function()
      {
       var mapping,list;
       mapping=function(grmName)
       {
        return[grmName,AjaxRemotingProvider.Sync("YC.Web:1",[{
         $:1
        },grmName])];
       };
       list=AjaxRemotingProvider.Sync("YC.Web:0",[{
        $:1
       }]);
       return _builder_.Bind(AlgorithmsComponents.InputAreaControl("Grammar",List.map(mapping,list)),function(_arg1)
       {
        return _builder_.Return(_arg1);
       });
      });
      x=Formlet.Vertical(formlet);
      inputRecord=FormContainerConfiguration.get_Default();
      CssClass={
       $:1,
       $0:"tomiddle"
      };
      fc=Runtime.New(FormContainerConfiguration,{
       Header:inputRecord.Header,
       Padding:inputRecord.Padding,
       Description:inputRecord.Description,
       BackgroundColor:inputRecord.BackgroundColor,
       BorderColor:inputRecord.BorderColor,
       CssClass:CssClass,
       Style:inputRecord.Style
      });
      GrammarInputForm=Enhance.WithCustomFormContainer(fc,x);
      _builder_1=Formlet.Do();
      formlet1=_builder_1.Delay(function()
      {
       var mapping,list;
       mapping=function(grmName)
       {
        return[grmName,AjaxRemotingProvider.Sync("YC.Web:1",[{
         $:0
        },grmName])];
       };
       list=AjaxRemotingProvider.Sync("YC.Web:0",[{
        $:0
       }]);
       return _builder_1.Bind(AlgorithmsComponents.InputAreaControl("Graph",List.map(mapping,list)),function(_arg2)
       {
        var formlet2,formlet3;
        formlet2=Controls.Checkbox(false);
        formlet3=Enhance.WithTextLabel("Show formal subgraph",formlet2);
        return _builder_1.Bind(Enhance.WithLabelLeft(formlet3),function(_arg3)
        {
         var formlet4,formlet5;
         formlet4=Controls.Checkbox(false);
         formlet5=Enhance.WithTextLabel("Remove redundant nodes",formlet4);
         return _builder_1.Bind(Enhance.WithLabelLeft(formlet5),function(_arg4)
         {
          return _builder_1.Return([_arg2,_arg3,_arg4]);
         });
        });
       });
      });
      x1=Formlet.Vertical(formlet1);
      inputRecord1=FormContainerConfiguration.get_Default();
      CssClass1={
       $:1,
       $0:"tomiddle"
      };
      fc1=Runtime.New(FormContainerConfiguration,{
       Header:inputRecord1.Header,
       Padding:inputRecord1.Padding,
       Description:inputRecord1.Description,
       BackgroundColor:inputRecord1.BackgroundColor,
       BorderColor:inputRecord1.BorderColor,
       CssClass:CssClass1,
       Style:inputRecord1.Style
      });
      GraphInputForm=Enhance.WithCustomFormContainer(fc1,x1);
      formlet6=Data.$(Data.$(Formlet.Return(function(grmInput)
      {
       return function(grphInput)
       {
        return[grmInput,grphInput];
       };
      }),GrammarInputForm),GraphInputForm);
      x2=Formlet.Horizontal(formlet6);
      inputRecord2=FormContainerConfiguration.get_Default();
      CssClass2={
       $:1,
       $0:"totop"
      };
      fc2=Runtime.New(FormContainerConfiguration,{
       Header:inputRecord2.Header,
       Padding:inputRecord2.Padding,
       Description:inputRecord2.Description,
       BackgroundColor:inputRecord2.BackgroundColor,
       BorderColor:inputRecord2.BorderColor,
       CssClass:CssClass2,
       Style:inputRecord2.Style
      });
      InputForm=Enhance.WithCustomFormContainer(fc2,x2);
      OutputForm=function(tupledArg)
      {
       var grammar,_arg5,subgraphCheckbox,removeCheckbox,graph,_builder_2,formlet2,formlet3,VisualizationForm,_builder_3,x3,inputRecord3,buttonConf,x4,x5,inputRecord4,CssClass3,fc3,RangeAndButtonForm,_builder_4,formlet4,VisualizationWithRangeForm,PathsVisualizationForm,_builder_6,formlet8;
       grammar=tupledArg[0];
       _arg5=tupledArg[1];
       subgraphCheckbox=_arg5[1];
       removeCheckbox=_arg5[2];
       graph=_arg5[0];
       _builder_2=Formlet.Do();
       formlet2=_builder_2.Delay(function()
       {
        var matchValue,_,tree,graph1,msg;
        matchValue=AjaxRemotingProvider.Sync("YC.Web:2",[grammar,graph,subgraphCheckbox,removeCheckbox]);
        if(matchValue.$==0)
         {
          tree=matchValue.$0;
          graph1=matchValue.$1;
          _=_builder_2.Bind(AlgorithmsComponents.ShowGraphImageControl("Graph Visualization",graph1,"canvas1"),function(_arg8)
          {
           return _builder_2.Bind(GraphParsingClient.ShowTreeImageControl("SPPF",tree,"canvas2"),function(_arg9)
           {
            return _builder_2.Return([_arg8,_arg9]);
           });
          });
         }
        else
         {
          msg=matchValue.$0;
          _=_builder_2.Bind(AlgorithmsComponents.OutputAreaControl("Error:"+msg,"Graph Visualization"),function(_arg6)
          {
           return _builder_2.Bind(AlgorithmsComponents.OutputAreaControl("Error:"+msg,"SPPF"),function(_arg7)
           {
            return _builder_2.Return([_arg6,_arg7]);
           });
          });
         }
        return _;
       });
       formlet3=Enhance.WithFormContainer(formlet2);
       VisualizationForm=Formlet.Horizontal(formlet3);
       _builder_3=Formlet.Do();
       x3=_builder_3.Delay(function()
       {
        return _builder_3.Bind(AlgorithmsComponents.RangeControl("Vertices","Initial","Final"),function(_arg10)
        {
         return _builder_3.Return(_arg10);
        });
       });
       inputRecord3=FormButtonConfiguration.get_Default();
       buttonConf=Runtime.New(FormButtonConfiguration,{
        Label:{
         $:1,
         $0:"FIND PATH"
        },
        Style:{
         $:1,
         $0:AlgorithmsComponents.buttonStyle()
        },
        Class:inputRecord3.Class
       });
       x4=Enhance.WithCustomSubmitButton(buttonConf,x3);
       x5=Formlet.Horizontal(x4);
       inputRecord4=FormContainerConfiguration.get_Default();
       CssClass3={
        $:1,
        $0:"todown"
       };
       fc3=Runtime.New(FormContainerConfiguration,{
        Header:inputRecord4.Header,
        Padding:inputRecord4.Padding,
        Description:inputRecord4.Description,
        BackgroundColor:inputRecord4.BackgroundColor,
        BorderColor:inputRecord4.BorderColor,
        CssClass:CssClass3,
        Style:inputRecord4.Style
       });
       RangeAndButtonForm=Enhance.WithCustomFormContainer(fc3,x5);
       _builder_4=Formlet.Do();
       formlet4=_builder_4.Delay(function()
       {
        return _builder_4.Bind(VisualizationForm,function(_arg11)
        {
         return _builder_4.Bind(RangeAndButtonForm,function(_arg12)
         {
          return _builder_4.Return([_arg11,_arg12]);
         });
        });
       });
       VisualizationWithRangeForm=Formlet.Vertical(formlet4);
       PathsVisualizationForm=function(rng)
       {
        var _builder_5,formlet5,formlet7;
        _builder_5=Formlet.Do();
        formlet5=_builder_5.Delay(function()
        {
         var _,matchValue,_1,tree,graph1,msg;
         if((rng[0]<rng[1]?rng[0]>=0:false)?rng[1]>=0:false)
          {
           matchValue=AjaxRemotingProvider.Sync("YC.Web:3",[grammar,graph,removeCheckbox,rng[0],rng[1]]);
           if(matchValue.$==0)
            {
             tree=matchValue.$0;
             graph1=matchValue.$1;
             _1=_builder_5.Bind(AlgorithmsComponents.ShowGraphImageControl("Path",graph1,"canvas3"),function(_arg15)
             {
              return _builder_5.Bind(GraphParsingClient.ShowTreeImageControl("SPPF Path",tree,"canvas4"),function(_arg16)
              {
               return _builder_5.Return([_arg15,_arg16]);
              });
             });
            }
           else
            {
             msg=matchValue.$0;
             _1=_builder_5.Bind(AlgorithmsComponents.OutputAreaControl("Error:"+msg,"Path"),function(_arg13)
             {
              return _builder_5.Bind(AlgorithmsComponents.OutputAreaControl("Error:"+msg,"SPPF Path"),function(_arg14)
              {
               return _builder_5.Return([_arg13,_arg14]);
              });
             });
            }
           _=_1;
          }
         else
          {
           _=_builder_5.Bind(AlgorithmsComponents.OutputAreaControl("Error: Incorrect range","Path"),function(_arg17)
           {
            return _builder_5.Bind(AlgorithmsComponents.OutputAreaControl("Error: Incorrect range","SPPF Path"),function(_arg18)
            {
             return _builder_5.Return([_arg17,_arg18]);
            });
           });
          }
         return _;
        });
        formlet7=Enhance.WithFormContainer(formlet5);
        return Formlet.Horizontal(formlet7);
       };
       _builder_6=Formlet.Do();
       formlet8=_builder_6.Delay(function()
       {
        return _builder_6.Bind(VisualizationWithRangeForm,function(_arg19)
        {
         return _builder_6.Bind(PathsVisualizationForm(_arg19[1]),function(_arg20)
         {
          return _builder_6.Return([_arg19,_arg20]);
         });
        });
       });
       return Formlet.Vertical(formlet8);
      };
      _builder_7=Formlet.Do();
      formlet9=_builder_7.Delay(function()
      {
       return _builder_7.Bind(InputForm,function(_arg21)
       {
        return _builder_7.Bind(OutputForm(_arg21),function(_arg22)
        {
         return _builder_7.Return(_arg22);
        });
       });
      });
      return Formlet.Vertical(formlet9);
     }),
     MainFormRun:function()
     {
      var Form,arg10,arg101;
      Form=GraphParsingClient.MainForm().Run(function()
      {
       return null;
      });
      arg101=List.ofArray([Form]);
      arg10=List.ofArray([Operators.add(Tags.Tags().NewTag("div",arg101),List.ofArray([Attr.Attr().NewAttr("align","center")]))]);
      return Tags.Tags().NewTag("div",arg10);
     },
     SPPF:function(lbl,g,c,canvas)
     {
      var hw,x,button,value,arg00,arg001,arg10;
      hw="height: "+AlgorithmsComponents.formH()+"; width: "+AlgorithmsComponents.formW();
      x=List.ofArray([Tags.Tags().text(lbl),Attr.Attr().NewAttr("style",hw)]);
      button=Tags.Tags().NewTag("button",x);
      arg00=function()
      {
       return function()
       {
        canvas["HtmlProvider@33"].Clear(canvas.get_Body());
        return((window.draw.call(null,((window.createTree.call(null,g))(c))(canvas.get_Id())))(canvas.get_Id()))(AlgorithmsComponents.graphSize());
       };
      };
      EventsPervasives.Events().OnClick(arg00,canvas);
      value=canvas;
      arg001=function()
      {
       return function()
       {
        ((window.draw.call(null,((window.createTree.call(null,g))(c))(canvas.get_Id())))(canvas.get_Id()))(AlgorithmsComponents.graphSize());
        return button["HtmlProvider@33"].Remove(button.get_Body());
       };
      };
      EventsPervasives.Events().OnClick(arg001,button);
      arg10=List.ofArray([canvas,button]);
      return Tags.Tags().NewTag("div",arg10);
     },
     ShowTreeImageControl:function(lbl,tree,id)
     {
      var formlet,formlet1,formlet2;
      formlet=Formlet.OfElement(function()
      {
       var arg10;
       arg10=List.ofArray([Attr.Attr().NewAttr("id",id)]);
       return GraphParsingClient.SPPF(lbl,tree.edges,tree.countOfVertex,Tags.Tags().NewTag("div",arg10));
      });
      formlet1=Enhance.WithTextLabel(lbl,formlet);
      formlet2=Enhance.WithLabelAbove(formlet1);
      return Enhance.WithFormContainer(formlet2);
     }
    },
    WebComponents:{
     AlgorithmsComponents:{
      ChooseDefaultControl:function(defaultData)
      {
       var ChoouseDefaultControlWidth,ChoouseDefaultControlHeight,_builder_;
       ChoouseDefaultControlWidth=AlgorithmsComponents.setFormWidth(0.15);
       ChoouseDefaultControlHeight=AlgorithmsComponents.setFormWidth(0.02);
       _builder_=Formlet.Do();
       return _builder_.Delay(function()
       {
        var formlet,x,tupledArg,height,width,x1;
        formlet=Controls.Select(1,Runtime.New(T,{
         $:1,
         $0:["",""],
         $1:defaultData
        }));
        x=Enhance.WithTextLabel("ChooseDefault",formlet);
        tupledArg=[ChoouseDefaultControlHeight,ChoouseDefaultControlWidth];
        height=tupledArg[0];
        width=tupledArg[1];
        x1=AlgorithmsComponents.setFormSize(height,width,"select",x);
        return _builder_.Bind(Enhance.WithFormContainer(x1),function(_arg1)
        {
         return _builder_.Return(_arg1);
        });
       });
      },
      FileControl:Runtime.Field(function()
      {
       var f,formlet;
       f=function()
       {
        var stateChanged,x,x1,arg00,input,reset;
        stateChanged=FSharpEvent.New();
        x=List.ofArray([Attr.Attr().NewAttr("type","file"),Attr.Attr().NewAttr("accept","text/*")]);
        x1=Tags.Tags().NewTag("input",x);
        arg00=function(el)
        {
         return function()
         {
          var file,reader;
          file=el.Dom.files.item(0);
          reader=new FileReader();
          reader.readAsText(file);
          return reader.addEventListener("load",function()
          {
           return stateChanged.event.Trigger(Runtime.New(Result,{
            $:0,
            $0:reader.result
           }));
          },true);
         };
        };
        EventsPervasives.Events().OnChange(arg00,x1);
        input=x1;
        reset=function()
        {
         input.set_Value("");
         return stateChanged.event.Trigger(Runtime.New(Result,{
          $:0,
          $0:""
         }));
        };
        return[input,reset,stateChanged.event];
       };
       formlet=Formlet.BuildFormlet(f);
       return Formlet.InitWith("",formlet);
      }),
      Graph:function(lbl,g,c,canvas)
      {
       var hw,x,button,arg00,arg001,arg10;
       hw="height: "+AlgorithmsComponents.formH()+"; width: "+AlgorithmsComponents.formW();
       x=List.ofArray([Tags.Tags().text(lbl),Attr.Attr().NewAttr("style",hw)]);
       button=Tags.Tags().NewTag("button",x);
       arg00=function()
       {
        return function()
        {
         canvas["HtmlProvider@33"].Clear(canvas.get_Body());
         return((window.draw.call(null,((window.createGraph.call(null,g))(c))(canvas.get_Id())))(canvas.get_Id()))(AlgorithmsComponents.graphSize());
        };
       };
       EventsPervasives.Events().OnClick(arg00,canvas);
       arg001=function()
       {
        return function()
        {
         ((window.draw.call(null,((window.createGraph.call(null,g))(c))(canvas.get_Id())))(canvas.get_Id()))(AlgorithmsComponents.graphSize());
         return button["HtmlProvider@33"].Remove(button.get_Body());
        };
       };
       EventsPervasives.Events().OnClick(arg001,button);
       arg10=List.ofArray([canvas,button]);
       return Tags.Tags().NewTag("div",arg10);
      },
      InputAreaControl:function(signature,defaultData)
      {
       var _builder_,formlet,formlet4,formlet5;
       _builder_=Formlet.Do();
       formlet=_builder_.Delay(function()
       {
        var _builder_1,formlet1;
        _builder_1=Formlet.Do();
        formlet1=_builder_1.Delay(function()
        {
         return _builder_1.Bind(AlgorithmsComponents.ChooseDefaultControl(defaultData),function(_arg1)
         {
          return _builder_1.Bind(AlgorithmsComponents.FileControl(),function(_arg2)
          {
           return _builder_1.Return([_arg1,_arg2]);
          });
         });
        });
        return _builder_.Bind(Formlet.Horizontal(formlet1),function(_arg3)
        {
         var fileInput,defaultValue,txt,formlet2,formlet3,x,tupledArg,height,width;
         fileInput=_arg3[1];
         defaultValue=_arg3[0];
         txt=fileInput===""?defaultValue:fileInput;
         formlet2=Controls.TextArea(txt);
         formlet3=Enhance.WithTextLabel(signature,formlet2);
         x=Enhance.WithLabelAbove(formlet3);
         tupledArg=[AlgorithmsComponents.formH(),AlgorithmsComponents.formW()];
         height=tupledArg[0];
         width=tupledArg[1];
         return _builder_.Bind(AlgorithmsComponents.setFormSize(height,width,"textarea",x),function(_arg4)
         {
          return _builder_.Return(_arg4);
         });
        });
       });
       formlet4=Formlet.FlipBody(formlet);
       formlet5=Formlet.Vertical(formlet4);
       return Enhance.WithFormContainer(formlet5);
      },
      OutputAreaControl:function(outputText,signature)
      {
       var _builder_,formlet;
       _builder_=Formlet.Do();
       formlet=_builder_.Delay(function()
       {
        var formlet1,formlet2,x,tupledArg,height,width;
        formlet1=Formlet.OfElement(function()
        {
         var arg10;
         arg10=List.ofArray([Attr.Attr().NewAttr("readonly","readonly"),Tags.Tags().text(outputText)]);
         return Tags.Tags().NewTag("textarea",arg10);
        });
        formlet2=Enhance.WithTextLabel(signature,formlet1);
        x=Enhance.WithLabelAbove(formlet2);
        tupledArg=[AlgorithmsComponents.formH(),AlgorithmsComponents.formW()];
        height=tupledArg[0];
        width=tupledArg[1];
        return _builder_.Bind(AlgorithmsComponents.setFormSize(height,width,"textarea",x),function(_arg1)
        {
         return _builder_.Return(_arg1);
        });
       });
       return Enhance.WithFormContainer(formlet);
      },
      RangeControl:function(signature,initLabel,finLabel)
      {
       var RangeControlWidth,RangeControlHeight,formlet,_builder_,_builder_1,formlet2,formlet3,formlet4;
       RangeControlWidth=Global.String(+AlgorithmsComponents.screenHeight()*0.16)+"px";
       RangeControlHeight=Global.String(+AlgorithmsComponents.screenHeight()*0.04)+"px";
       _builder_=Formlet.Do();
       _builder_1=Formlet.Do();
       formlet=Data.$(Data.$(Formlet.Return(function(min)
       {
        return function(max)
        {
         return[min<<0,max<<0];
        };
       }),_builder_.Delay(function()
       {
        var formlet1,x,tupledArg,height,width;
        formlet1=Controls.Input("");
        x=Enhance.WithTextLabel(initLabel,formlet1);
        tupledArg=[RangeControlHeight,RangeControlWidth];
        height=tupledArg[0];
        width=tupledArg[1];
        return _builder_.Bind(AlgorithmsComponents.setFormSize(height,width,"input",x),function(_arg1)
        {
         return _builder_.Return(_arg1);
        });
       })),_builder_1.Delay(function()
       {
        var formlet1,x,tupledArg,height,width;
        formlet1=Controls.Input("");
        x=Enhance.WithTextLabel(finLabel,formlet1);
        tupledArg=[RangeControlHeight,RangeControlWidth];
        height=tupledArg[0];
        width=tupledArg[1];
        return _builder_1.Bind(AlgorithmsComponents.setFormSize(height,width,"input",x),function(_arg2)
        {
         return _builder_1.Return(_arg2);
        });
       }));
       formlet2=Formlet.Horizontal(formlet);
       formlet3=Enhance.WithTextLabel(signature,formlet2);
       formlet4=Enhance.WithLabelAbove(formlet3);
       return Enhance.WithFormContainer(formlet4);
      },
      ShowGraphImageControl:function(lbl,graph,id)
      {
       var formlet,formlet1,formlet2;
       formlet=Formlet.OfElement(function()
       {
        var arg10;
        arg10=List.ofArray([Attr.Attr().NewAttr("id",id)]);
        return AlgorithmsComponents.Graph(lbl,graph.edges,graph.countOfVertex,Tags.Tags().NewTag("div",arg10));
       });
       formlet1=Enhance.WithTextLabel(lbl,formlet);
       formlet2=Enhance.WithLabelAbove(formlet1);
       return Enhance.WithFormContainer(formlet2);
      },
      buttonStyle:Runtime.Field(function()
      {
       return"padding-top: 0px;\r\n                        background-color: #FF69B4; \r\n                        border-width: 3px; \r\n                        font-weight: bold;\r\n                        border-color: #000000; \r\n                        border-radius: 10px; \r\n                        color: #000000; \r\n                        height: "+AlgorithmsComponents.setFormHeight(0.05)+"; \r\n                        width: "+AlgorithmsComponents.setFormWidth(0.06)+"; \r\n                        font-size:"+AlgorithmsComponents.setFormHeight(0.02);
      }),
      formH:Runtime.Field(function()
      {
       return AlgorithmsComponents.setFormHeight(0.15);
      }),
      formW:Runtime.Field(function()
      {
       return AlgorithmsComponents.setFormWidth(0.4);
      }),
      graphSize:Runtime.Field(function()
      {
       return Strings.Replace(AlgorithmsComponents.setFormWidth(0.4),"px","")<<0;
      }),
      screenHeight:Runtime.Field(function()
      {
       return jQuery("html").height();
      }),
      screenWidth:Runtime.Field(function()
      {
       return jQuery("html").width();
      }),
      setFormHeight:function(percent)
      {
       return Global.String(+AlgorithmsComponents.screenHeight()*percent)+"px";
      },
      setFormSize:function(height,width,formletType,formlet)
      {
       var f;
       f=function(e)
       {
        jQuery(e.Dom.querySelector(formletType)).css("height",height).css("width",width);
        return e;
       };
       return Formlet.MapElement(f,formlet);
      },
      setFormWidth:function(percent)
      {
       return Global.String(+AlgorithmsComponents.screenWidth()*percent)+"px";
      }
     }
    }
   }
  }
 });
 Runtime.OnInit(function()
 {
  Formlets=Runtime.Safe(Global.WebSharper.Formlets);
  Formlet=Runtime.Safe(Formlets.Formlet);
  YC=Runtime.Safe(Global.YC);
  Web=Runtime.Safe(YC.Web);
  WebComponents=Runtime.Safe(Web.WebComponents);
  AlgorithmsComponents=Runtime.Safe(WebComponents.AlgorithmsComponents);
  Remoting=Runtime.Safe(Global.WebSharper.Remoting);
  AjaxRemotingProvider=Runtime.Safe(Remoting.AjaxRemotingProvider);
  List=Runtime.Safe(Global.WebSharper.List);
  Enhance=Runtime.Safe(Formlets.Enhance);
  FormContainerConfiguration=Runtime.Safe(Enhance.FormContainerConfiguration);
  Controls=Runtime.Safe(Formlets.Controls);
  Data=Runtime.Safe(Formlets.Data);
  Strings=Runtime.Safe(Global.WebSharper.Strings);
  BioGraphClient=Runtime.Safe(Web.BioGraphClient);
  Html=Runtime.Safe(Global.WebSharper.Html);
  Client=Runtime.Safe(Html.Client);
  Tags=Runtime.Safe(Client.Tags);
  Array=Runtime.Safe(Global.Array);
  Arrays=Runtime.Safe(Global.WebSharper.Arrays);
  Attr=Runtime.Safe(Client.Attr);
  GraphParsingClient=Runtime.Safe(Web.GraphParsingClient);
  FormButtonConfiguration=Runtime.Safe(Enhance.FormButtonConfiguration);
  Operators=Runtime.Safe(Client.Operators);
  window=Runtime.Safe(Global.window);
  EventsPervasives=Runtime.Safe(Client.EventsPervasives);
  T=Runtime.Safe(List.T);
  Control=Runtime.Safe(Global.WebSharper.Control);
  FSharpEvent=Runtime.Safe(Control.FSharpEvent);
  FileReader=Runtime.Safe(Global.FileReader);
  IntelliFactory=Runtime.Safe(Global.IntelliFactory);
  Formlets1=Runtime.Safe(IntelliFactory.Formlets);
  Base=Runtime.Safe(Formlets1.Base);
  Result=Runtime.Safe(Base.Result);
  return jQuery=Runtime.Safe(Global.jQuery);
 });
 Runtime.OnLoad(function()
 {
  AlgorithmsComponents.screenWidth();
  AlgorithmsComponents.screenHeight();
  AlgorithmsComponents.graphSize();
  AlgorithmsComponents.formW();
  AlgorithmsComponents.formH();
  AlgorithmsComponents.buttonStyle();
  AlgorithmsComponents.FileControl();
  GraphParsingClient.MainForm();
  BioGraphClient.MainForm();
  return;
 });
}());
