{
    "$schema": "http://json-schema.org/draft-04/schema#",
    "type": "array",
    "items": [
        {
            "type": "object",
            "properties": {
                "modelName": {
                    "type": "string"
                },
                "processes": {
                    "type": "array",
                    "items": [
                        {
                            "type": "object",
                            "properties": {
                                "functionInputs": {
                                    "type": "array",
                                    "items": {
                                        "type": "string"
                                    }
                                },
                                "functionName": {
                                    "type": "string"
                                },
                                "functionParameters": {},
                                "processData": {
                                    "type": "processDataObject",
                                    "properties": {
                                        "StratumPolygon": {
                                            "type": "string"
                                        }
                                    },
                                    "required": [
                                        "StratumPolygon"
                                    ]
                                },
                                "processName": {
                                    "type": "string"
                                },
                                "processParameters": {
                                    "type": "object",
                                    "properties": {
                                        "enabled": {
                                            "type": "boolean"
                                        },
                                        "showInMap": {
                                            "type": "boolean"
                                        },
                                        "fileOutput": {
                                            "type": "boolean"
                                        }
                                    },
                                    "required": [
                                        "enabled",
                                        "showInMap",
                                        "fileOutput"
                                    ]
                                }
                            },
                            "required": [
                                "functionInputs",
                                "functionName",
                                "functionParameters",
                                "processData",
                                "processName",
                                "processParameters"
                            ]
                        }
                    ]
                }
            },
            "required": [
                "modelName",
                "processes"
                ]
        }, 
        "minItems": 0
        ]

    
    
    
    
    
    
    "definitions_processData_DefineStrata": {
        "DefineStrata": {
            "type": "string"
        }
    },
    
    "definitions_processData_AcousticPSU": {
        "AcousticPSU": {
            "type": "object", 
            "properties": {
                "Stratum_PSU": {
                    "type": "array",
                    "items": [
                        {
                            "name": "Stratum", 
                            "type": "string"
                        },
                        {
                            "name": "PSU", 
                            "type": "string"
                        }
                    ]
                }, 
                "EDSU_PSU": {
                    "type": "array",
                    "items": [
                        {
                            "name": "EDSU", 
                            "type": "string"
                        },
                        {
                            "name": "PSU", 
                            "type": "string"
                        }
                    ]
                }
            },
            "required": [
                "Stratum_PSU",
                "EDSU_PSU"
                ]
        }
    },
    
    
    "properties": {
        "processData_DefineStrata": { "$ref": "#/definitions_processData_DefineStrata/DefineStrata" },
        "processData_AcousticPSU": { "$ref": "#/definitions_processData_AcousticPSU/AcousticPSU" }
    }
    
}
