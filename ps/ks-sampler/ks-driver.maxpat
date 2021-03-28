{
	"patcher" : 	{
		"fileversion" : 1,
		"appversion" : 		{
			"major" : 8,
			"minor" : 1,
			"revision" : 10,
			"architecture" : "x64",
			"modernui" : 1
		}
,
		"classnamespace" : "box",
		"rect" : [ 416.0, 138.0, 605.0, 580.0 ],
		"bglocked" : 0,
		"openinpresentation" : 1,
		"default_fontsize" : 12.0,
		"default_fontface" : 0,
		"default_fontname" : "Arial",
		"gridonopen" : 1,
		"gridsize" : [ 15.0, 15.0 ],
		"gridsnaponopen" : 1,
		"objectsnaponopen" : 1,
		"statusbarvisible" : 2,
		"toolbarvisible" : 1,
		"lefttoolbarpinned" : 0,
		"toptoolbarpinned" : 0,
		"righttoolbarpinned" : 0,
		"bottomtoolbarpinned" : 0,
		"toolbars_unpinned_last_save" : 0,
		"tallnewobj" : 0,
		"boxanimatetime" : 200,
		"enablehscroll" : 1,
		"enablevscroll" : 1,
		"devicewidth" : 0.0,
		"description" : "",
		"digest" : "",
		"tags" : "",
		"style" : "",
		"subpatcher_template" : "",
		"assistshowspatchername" : 0,
		"boxes" : [ 			{
				"box" : 				{
					"id" : "obj-27",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "int" ],
					"patching_rect" : [ 90.0, 15.0, 60.0, 22.0 ],
					"text" : "midiin"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-26",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 134.5, 255.0, 61.0, 22.0 ],
					"text" : "peakreset"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-23",
					"maxclass" : "newobj",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "bang" ],
					"patching_rect" : [ 134.5, 225.0, 69.0, 22.0 ],
					"text" : "metro 2000"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-20",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "bang" ],
					"patching_rect" : [ 15.0, 15.0, 58.0, 22.0 ],
					"text" : "loadbang"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-19",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "int" ],
					"patcher" : 					{
						"fileversion" : 1,
						"appversion" : 						{
							"major" : 8,
							"minor" : 1,
							"revision" : 10,
							"architecture" : "x64",
							"modernui" : 1
						}
,
						"classnamespace" : "box",
						"rect" : [ 516.0, 223.0, 640.0, 480.0 ],
						"bglocked" : 0,
						"openinpresentation" : 0,
						"default_fontsize" : 12.0,
						"default_fontface" : 0,
						"default_fontname" : "Arial",
						"gridonopen" : 1,
						"gridsize" : [ 15.0, 15.0 ],
						"gridsnaponopen" : 1,
						"objectsnaponopen" : 1,
						"statusbarvisible" : 2,
						"toolbarvisible" : 1,
						"lefttoolbarpinned" : 0,
						"toptoolbarpinned" : 0,
						"righttoolbarpinned" : 0,
						"bottomtoolbarpinned" : 0,
						"toolbars_unpinned_last_save" : 0,
						"tallnewobj" : 0,
						"boxanimatetime" : 200,
						"enablehscroll" : 1,
						"enablevscroll" : 1,
						"devicewidth" : 0.0,
						"description" : "",
						"digest" : "",
						"tags" : "",
						"style" : "",
						"subpatcher_template" : "",
						"assistshowspatchername" : 0,
						"boxes" : [ 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-11",
									"index" : 1,
									"maxclass" : "outlet",
									"numinlets" : 1,
									"numoutlets" : 0,
									"patching_rect" : [ 30.0, 300.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-10",
									"maxclass" : "newobj",
									"numinlets" : 1,
									"numoutlets" : 1,
									"outlettype" : [ "" ],
									"patching_rect" : [ 60.0, 180.0, 72.0, 22.0 ],
									"text" : "prepend set"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-9",
									"maxclass" : "newobj",
									"numinlets" : 2,
									"numoutlets" : 1,
									"outlettype" : [ "int" ],
									"patching_rect" : [ 30.0, 135.0, 29.5, 22.0 ],
									"text" : "i 0"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-8",
									"maxclass" : "newobj",
									"numinlets" : 2,
									"numoutlets" : 1,
									"outlettype" : [ "int" ],
									"patching_rect" : [ 180.0, 255.0, 29.5, 22.0 ],
									"text" : "- 1"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-7",
									"maxclass" : "newobj",
									"numinlets" : 2,
									"numoutlets" : 1,
									"outlettype" : [ "int" ],
									"patching_rect" : [ 135.0, 255.0, 29.5, 22.0 ],
									"text" : "+ 1"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-6",
									"maxclass" : "newobj",
									"numinlets" : 3,
									"numoutlets" : 3,
									"outlettype" : [ "", "", "" ],
									"patching_rect" : [ 135.0, 75.0, 109.0, 22.0 ],
									"text" : "route 1 0"
								}

							}
, 							{
								"box" : 								{
									"comment" : "1 to add a voice, 0 to subtract",
									"id" : "obj-5",
									"index" : 1,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "" ],
									"patching_rect" : [ 135.0, 15.0, 30.0, 30.0 ]
								}

							}
 ],
						"lines" : [ 							{
								"patchline" : 								{
									"destination" : [ "obj-7", 0 ],
									"midpoints" : [ 69.5, 225.0, 144.5, 225.0 ],
									"order" : 1,
									"source" : [ "obj-10", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-8", 0 ],
									"midpoints" : [ 69.5, 225.0, 189.5, 225.0 ],
									"order" : 0,
									"source" : [ "obj-10", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-6", 0 ],
									"source" : [ "obj-5", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-7", 0 ],
									"source" : [ "obj-6", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-8", 0 ],
									"source" : [ "obj-6", 1 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-9", 0 ],
									"midpoints" : [ 144.5, 299.0, 225.0, 299.0, 225.0, 119.0, 39.5, 119.0 ],
									"source" : [ "obj-7", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-9", 0 ],
									"midpoints" : [ 189.5, 299.0, 225.0, 299.0, 225.0, 119.0, 39.5, 119.0 ],
									"source" : [ "obj-8", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-10", 0 ],
									"order" : 0,
									"source" : [ "obj-9", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-11", 0 ],
									"order" : 1,
									"source" : [ "obj-9", 0 ]
								}

							}
 ]
					}
,
					"patching_rect" : [ 216.5, 225.0, 91.0, 22.0 ],
					"saved_object_attributes" : 					{
						"description" : "",
						"digest" : "",
						"globalpatchername" : "",
						"tags" : ""
					}
,
					"text" : "p VoiceCounter"
				}

			}
, 			{
				"box" : 				{
					"drawpeaks" : 1,
					"ghostbar" : 31,
					"id" : "obj-14",
					"maxclass" : "multislider",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 217.0, 300.0, 15.0, 154.0 ],
					"peakcolor" : [ 1.0, 0.192156862745098, 0.192156862745098, 1.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 60.0, 0.0, 15.0, 154.0 ],
					"setminmax" : [ 0.0, 16.0 ],
					"settype" : 0,
					"spacing" : 2,
					"thickness" : 1
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-11",
					"maxclass" : "newobj",
					"numinlets" : 2,
					"numoutlets" : 2,
					"outlettype" : [ "signal", "signal" ],
					"patching_rect" : [ 135.0, 165.0, 60.0, 22.0 ],
					"text" : "plugout~"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-10",
					"maxclass" : "live.dial",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "float" ],
					"parameter_enable" : 1,
					"patching_rect" : [ 381.000000000000114, 45.0, 45.0, 54.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 0.0, 100.0, 45.0, 54.0 ],
					"saved_attribute_attributes" : 					{
						"valueof" : 						{
							"parameter_longname" : "Transpose",
							"parameter_mmax" : 24.0,
							"parameter_mmin" : -24.0,
							"parameter_shortname" : "Transpose",
							"parameter_type" : 1,
							"parameter_unitstyle" : 0
						}

					}
,
					"triangle" : 1,
					"varname" : "live.dial[2]"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-9",
					"maxclass" : "live.dial",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "float" ],
					"parameter_enable" : 1,
					"patching_rect" : [ 299.0, 45.0, 45.0, 48.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 0.0, 50.0, 45.0, 48.0 ],
					"saved_attribute_attributes" : 					{
						"valueof" : 						{
							"parameter_longname" : "Selector",
							"parameter_shortname" : "Selector",
							"parameter_type" : 0,
							"parameter_unitstyle" : 0
						}

					}
,
					"varname" : "live.dial[1]"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-8",
					"maxclass" : "live.dial",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "float" ],
					"parameter_enable" : 1,
					"patching_rect" : [ 217.0, 45.0, 45.0, 48.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 0.0, 0.0, 45.0, 48.0 ],
					"saved_attribute_attributes" : 					{
						"valueof" : 						{
							"parameter_longname" : "Chain",
							"parameter_shortname" : "Chain",
							"parameter_type" : 0,
							"parameter_unitstyle" : 0
						}

					}
,
					"varname" : "live.dial"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-7",
					"maxclass" : "newobj",
					"numinlets" : 4,
					"numoutlets" : 7,
					"outlettype" : [ "signal", "signal", "", "", "", "", "" ],
					"patching_rect" : [ 135.0, 120.0, 265.000000000000057, 22.0 ],
					"text" : "poly~ ks-voice 16 @target 0 @steal 0"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-5",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 90.0, 75.0, 101.0, 22.0 ],
					"text" : "prepend midinote"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-4",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 8,
					"outlettype" : [ "", "", "", "int", "int", "", "int", "" ],
					"patching_rect" : [ 90.0, 45.0, 92.5, 22.0 ],
					"text" : "midiparse"
				}

			}
 ],
		"lines" : [ 			{
				"patchline" : 				{
					"color" : [ 0.50195974111557, 0.0, 0.008130922913551, 1.0 ],
					"destination" : [ "obj-7", 3 ],
					"source" : [ "obj-10", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-14", 0 ],
					"source" : [ "obj-19", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-23", 0 ],
					"midpoints" : [ 24.5, 209.0, 144.0, 209.0 ],
					"source" : [ "obj-20", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-26", 0 ],
					"source" : [ "obj-23", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-14", 0 ],
					"source" : [ "obj-26", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-4", 0 ],
					"source" : [ "obj-27", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-5", 0 ],
					"source" : [ "obj-4", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-7", 0 ],
					"source" : [ "obj-5", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.50195974111557, 0.0, 0.008130922913551, 1.0 ],
					"destination" : [ "obj-10", 0 ],
					"midpoints" : [ 349.5, 157.0, 450.0, 157.0, 450.0, 30.0, 390.500000000000114, 30.0 ],
					"source" : [ "obj-7", 5 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-11", 1 ],
					"source" : [ "obj-7", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-11", 0 ],
					"source" : [ "obj-7", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-19", 0 ],
					"source" : [ "obj-7", 2 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.028541615232825, 0.250918865203857, 0.50196373462677, 1.0 ],
					"destination" : [ "obj-8", 0 ],
					"midpoints" : [ 267.500000000000057, 173.0, 466.0, 173.0, 466.0, 14.0, 226.5, 14.0 ],
					"source" : [ "obj-7", 3 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.065929934382439, 0.501798808574677, 0.006832201499492, 1.0 ],
					"destination" : [ "obj-9", 0 ],
					"midpoints" : [ 308.5, 166.0, 458.0, 166.0, 458.0, 22.0, 308.5, 22.0 ],
					"source" : [ "obj-7", 4 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.028541615232825, 0.250918865203857, 0.50196373462677, 1.0 ],
					"destination" : [ "obj-7", 1 ],
					"source" : [ "obj-8", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.065929934382439, 0.501798808574677, 0.006832201499492, 1.0 ],
					"destination" : [ "obj-7", 2 ],
					"source" : [ "obj-9", 0 ]
				}

			}
 ],
		"parameters" : 		{
			"obj-10" : [ "Transpose", "Transpose", 0 ],
			"obj-8" : [ "Chain", "Chain", 0 ],
			"obj-9" : [ "Selector", "Selector", 0 ],
			"parameterbanks" : 			{

			}
,
			"inherited_shortname" : 1
		}
,
		"dependency_cache" : [ 			{
				"name" : "ks-voice.maxpat",
				"bootpath" : "~/personal/repo/max-handpan/ps/ks-sampler",
				"patcherrelativepath" : ".",
				"type" : "JSON",
				"implicit" : 1
			}
, 			{
				"name" : "ks-sampler-core.maxpat",
				"bootpath" : "~/personal/repo/max-handpan/ps/ks-sampler",
				"patcherrelativepath" : ".",
				"type" : "JSON",
				"implicit" : 1
			}
, 			{
				"name" : "sc_mk1_range_id_selector.table",
				"bootpath" : "~/personal/repo/max-handpan/ps/ks-sampler",
				"patcherrelativepath" : ".",
				"type" : "TEXT",
				"implicit" : 1
			}
, 			{
				"name" : "sc_mk1_range_id_chain.table",
				"bootpath" : "~/personal/repo/max-handpan/ps/ks-sampler",
				"patcherrelativepath" : ".",
				"type" : "TEXT",
				"implicit" : 1
			}
, 			{
				"name" : "sc_mk1_range_id_velocity.table",
				"bootpath" : "~/personal/repo/max-handpan/ps/ks-sampler",
				"patcherrelativepath" : ".",
				"type" : "TEXT",
				"implicit" : 1
			}
, 			{
				"name" : "sc_mk1_samples_selector.coll",
				"bootpath" : "~/personal/repo/max-handpan/ps/ks-sampler",
				"patcherrelativepath" : ".",
				"type" : "TEXT",
				"implicit" : 1
			}
, 			{
				"name" : "sc_mk1_range_id_key.table",
				"bootpath" : "~/personal/repo/max-handpan/ps/ks-sampler",
				"patcherrelativepath" : ".",
				"type" : "TEXT",
				"implicit" : 1
			}
, 			{
				"name" : "sc_mk1_samples_chain.coll",
				"bootpath" : "~/personal/repo/max-handpan/ps/ks-sampler",
				"patcherrelativepath" : ".",
				"type" : "TEXT",
				"implicit" : 1
			}
, 			{
				"name" : "sc_mk1_samples_velocity.coll",
				"bootpath" : "~/personal/repo/max-handpan/ps/ks-sampler",
				"patcherrelativepath" : ".",
				"type" : "TEXT",
				"implicit" : 1
			}
, 			{
				"name" : "sc_mk1_samples_key.coll",
				"bootpath" : "~/personal/repo/max-handpan/ps/ks-sampler",
				"patcherrelativepath" : ".",
				"type" : "TEXT",
				"implicit" : 1
			}
, 			{
				"name" : "sc_mk1_samples.coll",
				"bootpath" : "~/personal/repo/max-handpan/ps/ks-sampler",
				"patcherrelativepath" : ".",
				"type" : "TEXT",
				"implicit" : 1
			}
 ],
		"autosave" : 0
	}

}
